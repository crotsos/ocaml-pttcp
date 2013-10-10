(*
 * Copyright (c) 2012 Charalmpos Rotsos <cr409@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Net
open Net.Nettypes
open Lwt 
open Printf 
open Rpc

let pr = Printf.printf

type pttcp_t = {
  (* req_id, step_id -> ((src_ip, size), (dst_ip * int) ) *)
  requests : (int * int, Rpc.t * ((Ipaddr.V4.t * float * int) list) * ((Ipaddr.V4.t * int) list) ) Hashtbl.t;
  push : ((Ipaddr.V4.t * int) list * Rpc.t) option -> unit;
}

let send_data json size t = 
  let data = Jsonrpc.to_string json in 
  let buf = Cstruct.sub (OS.Io_page.to_cstruct (OS.Io_page.get 1)) 0 ((String.length data) + 4) in
  let _i = Cstruct.LE.set_uint32 buf 0 (Int32.of_int (String.length data)) in 
  let _ = Cstruct.blit_from_string data 0 buf 4 (String.length data) in  

  let _ = Channel.write_buffer t buf in 
  lwt _ = Channel.flush t in 
  let rec send_data state t = function 
    | 0 -> return ()
    | len when (len > 1460) -> 
        let buf = (Cstruct.sub (OS.Io_page.to_cstruct (OS.Io_page.get 1)) 0 1460) in 
        let _ = Channel.write_buffer t buf in 
        lwt _ = Channel.flush t in 
          send_data state t (len - 1460)
    | len ->
        let buf = Cstruct.sub (OS.Io_page.to_cstruct (OS.Io_page.get 1)) 0 len in
        let _ = Channel.write_buffer t buf in 
          Channel.flush t 
  in 
    send_data state t size
 


let request_manage mgr json port (dest_ip, size) =  
  (* TODO add code client generating traffic here *)
  let _ = pr "request from %s %d bytes\n%!" (Ipaddr.V4.to_string dest_ip) size in 
  lwt _ = Net.Channel.connect mgr (`TCPv4 (None, (dest_ip, port), 
                                               send_data json size) ) in 
  let _ = printf "XXXXXXX returning from connection\n%!" in 
  return ()

let request_manager mgr stream port = 
  while_lwt true do
    lwt request = Lwt_stream.get stream in 
    match request with
    | Some ( requests, json) -> Lwt_list.iter_p  (request_manage mgr json port) requests
    | None -> return ()
  done 
(*{ req_id:1,step_id:2, init_ts:0.01, path: 
  * [
      [ 
        {src_ip:127.0.0.2, st_ip:127.0.0.1, delay:0.1, len:100},
        {src_ip:127.0.0.2, dst_ip:127.0.0.1, delay:0.1, len:100}, 
      ], 

  [ [{src_ip:127.0.0.2, st_ip:127.0.0.1, delay:0.1,
  len:100}],[{src_ip:127.0.0.2, dst_ip:127.0.0.1, delay:0.1, len:100}], ], 

  [ [{src_ip:127.0.0.2, st_ip:127.0.0.1, delay:0.1,
  len:100}],[{src_ip:127.0.0.2, dst_ip:127.0.0.1, delay:0.1, len:100}], ],  ]
 * *)

let rec get_field field = function
  | Dict t -> begin
      let rec get_field_inner = function
        | [] -> raise Not_found
        | (name, t)::_ when field = name -> t
        | _::rest -> get_field_inner rest
      in
      get_field_inner t
  end
  | _ -> raise Not_found
let rec get_list field t =
  match (get_field field t) with 
  | Enum t -> t
  | _ -> raise Not_found


let rec ipv4_of_field field lst = Ipaddr.V4.of_int32 (int32_of_rpc (get_field field lst))
let rec float_of_field field lst = float_of_rpc (get_field field lst)
let rec int_of_field field lst = int_of_rpc (get_field field lst)
let rec string_of_field field lst = string_of_rpc (get_field field lst)


let request_handler st src_ip (dst_ip, dst_port) t = 
  (* receive the initial json path description *)
  lwt buf = Channel.read_exactly t 4 in 
  let req_len = Int32.to_int (Cstruct.LE.get_uint32 buf 0) in 
  lwt json_buf = Channel.read_exactly t req_len in 
  let json_str = Cstruct.to_string json_buf in 
  let _ = printf "got a request as a server containing\n%s\n%!" json_str in 
  let json = 
    try
      Jsonrpc.of_string json_str
    with exn -> 
      let _ = eprintf "failed with error %s\n%!" (Printexc.to_string exn) in 
        failwith "error"
  in

  (* export some details *)
  let req_id = int_of_field "req_id" json in  
  let step_id = int_of_field "step_id" json in 


  (* translate request to input and output streams *)
  let (delay, size) = 
    if ( not (Hashtbl.mem st.requests (req_id, step_id))) then
      (* Haven't seen this request before, add state in hashtbl *)
      let path = List.nth (get_list "path" json) step_id in

      let srcs = 
        match (path) with
        | Enum t -> 
            begin
              List.fold_right (
                fun hop ret -> 
                  match hop with
                  | Dict l ->
                    if (ipv4_of_field "dst_ip" hop = src_ip) then 
                      let _ = pr "waiting %s\n%!" (string_of_field "dst_ip" hop) in
                      ((ipv4_of_field "src_ip" hop), 
                      (float_of_field "delay" hop), (int_of_field "len" hop))::ret 
                    else ret
                  | _ -> ret
              ) t []
            end
        | _ -> failwith "malformed path"
      in

      let dsts = 
        try 
          let path = List.nth (get_list "path" json) (step_id + 1) in
          match (path) with
          | Enum t -> 
              begin
                List.fold_right (
                  fun hop ret -> 
                    match hop with 
                    | Dict _ -> 
                      if ( ipv4_of_field "src_ip" hop = src_ip) then
                        ((ipv4_of_field "dst_ip" hop),(int_of_field "len" hop))::ret
                      else ret
                    | _ -> ret
                ) t []  
              end
          | _ -> failwith "malformed path" 
        with Failure "nth" -> []
      in

      let (_, delay, size) = List.find (fun (ip, _, _) -> ip = dst_ip) srcs in 
      let json = Dict  
        [("req_id", (rpc_of_int req_id) );
         ("step_id", (rpc_of_int (step_id + 1)) );
         ("init_ts", get_field "init_ts" json);
         ("path", get_field "path" json)] in 
      let _ = Hashtbl.replace st.requests (req_id, step_id) (json, srcs, dsts) in 
        (delay, size )
  else 
    (* I already got a requst before, I only need to calcualte how match data I
     * need to receive *)
    let (_, elem, _) = Hashtbl.find st.requests (req_id, step_id) in 
    let (_, delay, size) = List.find (fun (ip, _, _) -> ip = dst_ip) elem in
      (delay, size)
  in
  let rec read_data len t = 
    lwt buf = Net.Channel.read_some t in 
    match (len - (Cstruct.len buf)) with
    | len when (len <= 0) -> return ()
    | len -> read_data len t
  in

  let _ = printf "client reading %d bytes \n%!" size in 
  lwt _ = read_data size t in 
  let _ = printf "client read %d bytes \n%!" size in 
  (* Fake some processing *) 
  lwt _ = OS.Time.sleep delay in 

  (* Remove my request *)
  let (json, pending, requests ) = Hashtbl.find st.requests (req_id, step_id) in 
    if (List.length pending = 1) then 
      let _ = Hashtbl.remove st.requests (req_id, step_id) in 
      let _ = st.push (Some (requests, json)) in 
        return () 
    else 
      let pending = List.filter (fun (ip, _, _) -> ip <> dst_ip ) pending in 
      let _ = Hashtbl.replace st.requests (req_id, step_id) (json, pending,
      requests) in 
        return ()
   

let create_server mgr ip port =
  let (t, push) = Lwt_stream.create () in 
  let _ = Lwt.ignore_result (request_manager mgr t port) in 
  let st = {requests=(Hashtbl.create 24); push;} in 
  let _ = ignore_result (Net.Channel.listen mgr (`TCPv4 ((None, port),(request_handler st ip )))) in 
    st


let create_flow mgr st port desc = 
  let json = Jsonrpc.of_string desc in 
  (* export some details *)
  let step_id = int_of_field "step_id" json in  
  let path = List.nth (get_list "path" json) step_id in
  let dsts =
    match path with 
    | Enum t -> begin
        List.fold_right (
          fun hop ret -> 
            match hop with
            | Dict _ -> (ipv4_of_field "dst_ip" hop, (int_of_field "len" hop))::ret 
            | _ -> ret) t [] 
    end
    | _ -> failwith "Malformed request"
  in
  st.push (Some (dsts, json) )

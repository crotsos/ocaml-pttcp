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
open Tiny_json
open Util
open Json

type pttcp_t = {
  (* req_id, step_id -> ((src_ip, size), (dst_ip * int) ) *)
  requests : (int * int, Json.t * ((ipv4_addr  * float * int) list) * ((ipv4_addr * int) list) ) Hashtbl.t;
  push : ((ipv4_addr * int) list * Json.t) option -> unit;
}

let json_to_string =
  let rec show_aux depth = function
    | String s -> "\"" ^s^ "\""
    | Number x -> !%"%f" x
    | Object fs ->
        "{"^ slist (",") (fun (k,v) -> "\""^k^"\":"^ (show_aux (depth+1)) v) fs^"}"
    | Array xs -> "[" ^slist "," (show_aux depth) xs ^ "]"
    | Bool true -> "TRUE"
    | Bool false -> "FALSE"
    | Null -> "NULL"
  in
  show_aux 1


let send_data json size t = 
  let data = json_to_string json in 
  let buf = Cstruct.sub (OS.Io_page.to_cstruct (OS.Io_page.get ())) 0 ((String.length data) + 4) in
  let _i = Cstruct.LE.set_uint32 buf 0 (Int32.of_int (String.length data)) in 
  let _ = Cstruct.blit_from_string data 0 buf 4 (String.length data) in  

  let _ = Channel.write_buffer t buf in 
  lwt _ = Channel.flush t in 
  let rec send_data state t = function 
    | 0 -> return ()
    | len when (len > 1460) -> 
        let buf = (Cstruct.sub (OS.Io_page.to_cstruct (OS.Io_page.get ())) 0 1460) in 
        let _ = Channel.write_buffer t buf in 
        lwt _ = Channel.flush t in 
          send_data state t (len - 1460)
    | len ->
        let buf = Cstruct.sub (OS.Io_page.to_cstruct (OS.Io_page.get ())) 0 len in
        let _ = Channel.write_buffer t buf in 
          Channel.flush t 
  in 
    send_data state t size
 


let request_manage mgr json port (dest_ip, size) =  
  (* TODO add code client generating traffic here *)
  let _ = printf "XXXXX will request from dest_ip %s to send %d data \n%!"
  (Nettypes.ipv4_addr_to_string dest_ip) size in 
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
(*{ req_id:1,step_id:2, init_ts:0.01, path: [
  [ [{src_ip:127.0.0.2, st_ip:127.0.0.1, delay:0.1,
  len:100}],[{src_ip:127.0.0.2, dst_ip:127.0.0.1, delay:0.1, len:100}], ], 

  [ [{src_ip:127.0.0.2, st_ip:127.0.0.1, delay:0.1,
  len:100}],[{src_ip:127.0.0.2, dst_ip:127.0.0.1, delay:0.1, len:100}], ], 

  [ [{src_ip:127.0.0.2, st_ip:127.0.0.1, delay:0.1,
  len:100}],[{src_ip:127.0.0.2, dst_ip:127.0.0.1, delay:0.1, len:100}], ],  ]
 * *)

let rec get_object_field field = function
  | [] -> raise Not_found
  | (name, t)::_ when field = name -> t
  | _::rest -> get_object_field field rest
let rec get_object_field_as_ipv4 field lst = 
  Net.Nettypes.ipv4_addr_of_string (Json.as_string (get_object_field field lst))
let rec get_object_field_as_float field lst = Json.as_float (get_object_field field lst)
let rec get_object_field_as_int field lst = 
  int_of_float (Json.as_float (get_object_field field lst))


let request_handler st src_ip (dst_ip, dst_port) t = 
  (* receive the initial json path description *)
  lwt buf = Channel.read_exactly t 4 in 
  let req_len = Int32.to_int (Cstruct.LE.get_uint32 buf 0) in 
  lwt json_buf = Channel.read_exactly t req_len in 
  let json_str = Cstruct.to_string json_buf in 
  let _ = printf "got a request as a server containing\n%s\n%!" json_str in 
  let json = 
    try
      Json.as_object (Json.parse json_str) 
    with exn -> 
      let _ = eprintf "failed with error %s\n%!" (Printexc.to_string exn) in 
        failwith "error"
  in

  (* export some details *)
  let req_id = get_object_field_as_int "req_id" json in  
  let step_id = get_object_field_as_int "step_id" json in 


  (* translate request to input and output streams *)
  let (delay, size) = 
    if ( not (Hashtbl.mem st.requests (req_id, step_id))) then
      (* Haven't seen this request before, add state in hashtbl *)
      let path = List.nth (Json.as_list (get_object_field "path" json)) step_id in

      let srcs = List.fold_right (
        fun hop ret -> 
          let hop_obj = Json.as_object hop in 
          match (get_object_field_as_ipv4 "dst_ip" hop_obj, 
               get_object_field_as_ipv4 "src_ip" hop_obj) with
          | Some dst, Some src when dst = src_ip ->
              let _ = printf "waiting for dest %s\n%!" (Nettypes.ipv4_addr_to_string src) in 
              (src, (get_object_field_as_float "delay" hop_obj),
               (get_object_field_as_int "len" hop_obj))::ret 
          | _ -> ret
      ) (Json.as_list path) [] in 

      let dsts = 
        try 
          let path = List.nth (Json.as_list (get_object_field "path" json)) (step_id + 1) in
          List.fold_right (
            fun hop ret -> 
              let hop_obj = Json.as_object hop in 
              match (get_object_field_as_ipv4 "dst_ip" hop_obj, 
                     get_object_field_as_ipv4 "src_ip" hop_obj) with
              | Some dst, Some src when src = src_ip -> 
                  (dst,(get_object_field_as_int "len" hop_obj))::ret 
              | _ -> ret
                  ) (Json.as_list path) []  
        with Failure "nth" -> []
      in
      let (_, delay, size) = List.find (fun (ip, _, _) -> ip = dst_ip) srcs in 
      let json = Json.Object 
        [("req_id", Json.Number (float_of_int req_id) );
         ("step_id", Json.Number(float_of_int (step_id + 1)));
         ("init_ts", get_object_field "init_ts" json);
         ("path", get_object_field "path" json)] in 
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
  let json = Json.as_object (Json.parse desc) in 

  (* export some details *)
  let req_id = int_of_float (Json.as_float (get_object_field "req_id" json) ) in  
  let step_id = int_of_float (Json.as_float (get_object_field "step_id" json) ) in  

  let path = List.nth (Json.as_list (get_object_field "path" json)) step_id in
  let dsts = 
    List.fold_right (
      fun hop ret -> 
        let hop_obj = Json.as_object hop in 
        match (get_object_field_as_ipv4 "dst_ip" hop_obj, get_object_field_as_ipv4 "src_ip" hop_obj) with
          | Some dst, _ -> (dst,(get_object_field_as_int "len" hop_obj))::ret 
          | _ -> ret
    ) (Json.as_list path) []  
  in
  let _ = st.push (Some (dsts, (Json.Object json) )) in
    ()

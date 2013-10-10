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

(*
 * Traffic generation state description
 * *)

type state_t = {                                         
    sinme: ipv4_dst;
    sinhim: ipv4_dst;
    mutable tx_target: int32;        (* used by tx side *)
    mutable tx_pkts: int32;
    mutable tx_sent: int32;
    mutable tx_sent_cpt: int32;     (* bytes rx'd since checkpoint *)
    mutable tx_start: float;        (* send_data(): just as start sending *)
    mutable tx_stop: float;         (* send_data(): when all is sent *)
                                                                         
    mutable rx_pkts: int32;
    mutable rx_rcvd: int32;         (* used by rx side *)
    mutable rx_rcvd_cpt: int32;     (* bytes rx'd since checkpoint *)
    mutable rx_start: float;        (* sink_data(): when first bits rcvd *)
    mutable rx_stop: float;         (* sink_data(): when no bits rcvd *)
    
    (* surge requests per page *)
    mutable objperpage: float;
    (* state for handling more complex traffic generators *)
    mutable client_id: int;
}                                                       

let init_channel_state_t sinme sinhim client_id = 
  {sinme;  sinhim; tx_target=0l; client_id; objperpage=0.0; 
   tx_pkts=0l; tx_sent=0l; tx_sent_cpt=0l; tx_start=0.0; tx_stop=0.0;
   rx_pkts=0l; rx_rcvd=0l; rx_rcvd_cpt=0l; rx_start=0.0; rx_stop=0.0;}

let update_tx_stat state len = 
  state.tx_sent <- Int32.add state.tx_sent len;
  state.tx_sent_cpt <- Int32.add state.tx_sent_cpt len;
  state.tx_pkts <- Int32.add state.tx_pkts 1l;
  state.tx_stop <- (OS.Clock.time ())

let update_rx_stat state len = 
  state.rx_rcvd <- Int32.add state.rx_rcvd len;
  state.rx_rcvd_cpt <- Int32.add state.rx_rcvd_cpt len;
  state.rx_pkts <- Int32.add state.rx_pkts 1l;
  state.rx_stop <- (OS.Clock.time ())

let update_tx_target state len = 
  state.tx_target <- len;
  state.tx_sent <- 0l;
  state.tx_pkts <- 0l;
  state.tx_start <- (OS.Clock.time ());
  state.rx_rcvd <- 0l;
  state.rx_pkts <- 0l;
  state.rx_start <- (OS.Clock.time ())

type model = 
  | Constant of float
  | Exp of float 
  | Pareto of float * float

let get_sample = function
  | Constant(m) -> m
  | Exp(m) -> (abs_float (log ( 1.0 -. (Random.float 1.0) ))) /. m
  | Pareto(m, s) -> 
      let v = Random.float max_float in
      let d = v ** (1.0/.s) in 
        m *. ( 1.0 /. d)

type connection_model = 
(*  | Simple_rx of num_ports * base_port *)
  | Simple_rx of int * int
(*   | Simple_tx of num_conn * bytes * dhost * num_ports * base_port *)
  | Simple_tx of int * int32 * ipv4_addr * int * int
(*   | Svr of num_ports * base_port  *)
  | Srv of int * int
(*   | Simple_clt of n, bytes, dhost, num_ports, base_port *)
  | Simple_clt of int * int32 * ipv4_addr * int * int
(*   | Cts_ctl of n, bytes, dhost, num_ports, base_port *)
  | Cts_ctl of int * int32 * ipv4_addr * int * int
(*   | Surge_client of n, dhost, num_ports, base_port interpage objperpage interobj objsize *)
  | Surge_client of int * ipv4_addr * int * int * model * model * model * model 

type flow_model = 
  | TCP
  (* packet interarrival * packet size *)
  | UDP of model * int

type pttcp_t = {
  mutable states: state_t list;
  mutable finished: state_t list;
  verbose : bool;
  mode : connection_model;
  flow : flow_model;
  mutable max_id : int;}

let init_pttcp_state_t mode flow verbose = 
  { states=[]; finished=[]; mode; flow; max_id=0; verbose;}

let add_pttcp_state st src_port dst_ip dst_port = 
   let client_id = st.max_id in 
   let state = 
     init_channel_state_t 
      ((Nettypes.ipv4_addr_of_tuple (0l,0l,0l,0l)), src_port) 
      (dst_ip,dst_port) client_id in
   let _ =st.max_id <- st.max_id + 1 in
   let _ = st.states <- [state] @ st.states in 
     state 

let del_pttcp_state st state =
  let _ = st.finished <- st.finished @ [state] in
  let remaining = List.filter (fun s -> (not (s = state)) ) st.states in
    st.states <- remaining 

let print_pttcp_state_rx st = 
  while_lwt true do 
    lwt _ = OS.Time.sleep 1.0 in 
    let total_bytes = List.fold_right
      (fun s b ->
        let r = Int32.add s.rx_rcvd_cpt b in
        let rate = ((Int32.to_float s.rx_rcvd_cpt) *. 8.0) /. 1024.0 in 
        let _ = s.rx_rcvd_cpt <- 0l in
        let _ = printf "+%.4fkbps " rate in 
          r) st.states 0l in
    let progress_count = 
      List.length (List.filter (fun s -> (s.rx_rcvd_cpt > 0l)) st.states) in 
    let active_count = List.length st.states in
    let finished_bytes = List.fold_right
      (fun s b ->
        let r = Int32.add s.rx_rcvd_cpt b in 
        let rate = ((Int32.to_float s.rx_rcvd_cpt) *. 8.0) /. 1024.0 in 
        let _ = if (st.verbose) then printf "-%.4fkbps " rate in 
          r) st.finished 0l in
    let finished_count = List.length st.finished in 
    let _ = st.finished <- [] in 
    let rate = ((Int32.to_float (Int32.add total_bytes finished_bytes)) *. 8.0) /. 1048576.0 in 
    let _ = printf "\n%.02f server: %d streams active, %d made progress, %d finished: tot = %ld, tot Mb/s = %.2f\n%!"
              (OS.Clock.time ()) active_count progress_count finished_count (Int32.add total_bytes finished_bytes) rate in
      return ()
  done

let print_pttcp_state_tx st = 
  while_lwt true do 
    lwt _ = OS.Time.sleep 1.0 in
    let total_bytes = List.fold_right
      (fun s b ->
        let r = Int32.add s.tx_sent_cpt b in
        let rate = ((Int32.to_float s.tx_sent_cpt) *. 8.0) /. 1024.0 in 
        let _ = s.tx_sent_cpt <- 0l in
        let _ = printf "+%.4fkbps " rate in 
          r) st.states 0l in
    let progress_count = 
      List.length (List.filter (fun s -> (s.tx_sent_cpt > 0l)) st.states) in 
    let active_count = List.length st.states in
    let finished_bytes = List.fold_right
      (fun s b ->
        let r = Int32.add s.tx_sent_cpt b in 
        let rate = ((Int32.to_float s.tx_sent_cpt) *. 8.0) /. 1024.0 in 
        let _ = if (st.verbose) then printf "-%.4fkbps " rate in 
          r) st.finished 0l in
    let finished_count = List.length st.finished in 
    let _ = st.finished <- [] in 
    let rate = ((Int32.to_float (Int32.add total_bytes finished_bytes)) *. 8.0) /. 1048576.0 in 
    let _ = printf "\n%.02f client: %d streams active, %d made progress, %d finished: tot = %ld, tot Mb/s = %.2f\n%!"
              (OS.Clock.time ()) active_count progress_count finished_count (Int32.add total_bytes finished_bytes) rate in
      return ()
  done 

(*
 * util methods
 * *)
let write_and_flush mgr src_port dst buf = Net.Datagram.UDPv4.send mgr ~src:(None, src_port) dst buf

(*
 * Listening methods
 * *)

let create_listeners mgr st num_ports base_port cb = 
    let rec port_num_list = function 
      | num when (num = base_port) -> [num]
      | num -> [num] @ (port_num_list (num-1))
    in
    let ports = port_num_list (base_port + num_ports) in 
      (Lwt_list.iter_p (
        fun port -> 
          try_lwt
            let _ = printf "setting up a listener on port %d...\n%!" port in 
              Net.Datagram.UDPv4.recv mgr (None, port) (cb mgr st port) 
        with exn ->
          return (eprintf "%03.f: create_listeners error : %s\n%!" 
                    (OS.Clock.time ()) (Printexc.to_string exn))

      ) ports) <&> (print_pttcp_state_tx st)

let unmarshal_flow_request buf = 
  let tx_target = Cstruct.LE.get_uint32 buf 0 in 
  let model = 
    match (Cstruct.LE.get_uint16 buf 4) with
    | 6 -> TCP
    | 17 -> begin 
      let delay = Cstruct.LE.get_uint32 buf 8 in 
      let delay = ((Int32.to_float delay) /. 1000000.0) in 
      let size = Cstruct.LE.get_uint16 buf 12 in 
      match (Cstruct.LE.get_uint16 buf 6) with
      | 1 -> UDP((Constant delay), size)
      | 2 -> UDP((Exp(delay)), size)
      | typ -> failwith (sprintf "Invalid model type %d" typ)  
    end 
    | typ -> failwith (sprintf "Invalid flow type %d" typ)
  in
  (tx_target, model)

let simple_server mgr st src_port (dst_ip, dst_port) buf = 
  let state = add_pttcp_state st src_port dst_ip dst_port in 
  try_lwt
      let _ = printf "got a request on port %d\n%!" src_port in 
      let (tx_len, fl) = unmarshal_flow_request buf in 
      let (delay, size) = 
        match fl with
        | TCP -> ((Constant(0.0)), 1460l)
        | UDP (model, size) -> (model, (Int32.of_int size))
      in
      let _ = update_tx_target state tx_len in
      let rec send_data state = function 
        | 0l -> return ()
        | len when (len > size) ->
            let delay = get_sample delay in 
            lwt _ = OS.Time.sleep delay in 
            let buf = (Cstruct.sub (OS.Io_page.to_cstruct (OS.Io_page.get ())) 0
            (Int32.to_int size)) in 
            lwt _ = write_and_flush mgr src_port (dst_ip, dst_port) buf in
            let _ = update_tx_stat state size in
              send_data state (Int32.sub len size)
        | len ->
            lwt _ = OS.Time.sleep (get_sample delay) in 
            let buf = Cstruct.sub (OS.Io_page.to_cstruct (OS.Io_page.get ())) 0 (Int32.to_int len) in 
            lwt _ = write_and_flush mgr src_port (dst_ip, dst_port) buf in 
            let _ = update_tx_stat state len in
            let _ = if (st.verbose) then 
              eprintf "%03.6f: flow %d - finished %ld bytes (%ld pkts)\n%!"
                   (OS.Clock.time ()) state.client_id state.tx_target state.tx_pkts
            in
            let _ = del_pttcp_state st state in 
              return ()
      in 
      let _ = Lwt.ignore_result (send_data state tx_len) in
        return ()
  with exn ->
    let _ = del_pttcp_state st state in 
    return (eprintf "%03.f: simple_server error : %s\n%!" 
              (OS.Clock.time ()) (Printexc.to_string exn))

(*
 * request generating methods 
 * *)
let create_connectors mgr st dhost num_ports base_port conns continuous cb = 
    let rec port_num_list = function 
      | num when (num = 0) -> []
      | num -> [base_port + (num mod num_ports) ] @ (port_num_list (num-1))
    in
    let ports = port_num_list conns in 
      (Lwt_list.iter_p (
        fun port -> 
          let count = ref 0 in 
          let state = add_pttcp_state st 8888 dhost port in 
          lwt _ = 
            while_lwt ((!count < 1) || continuous) do
              count := !count + 1;
              let _ = printf "connecting to UDP server\n%!" in
                  
(*                  Net.Datagram.UDPv4.recv mgr (dhost, port) *)
                cb state mgr port dhost port
            done 
          in
          let _ = del_pttcp_state st state in 
            return ()
      ) ports) <&> (print_pttcp_state_rx st)

let marshal_flow_request state flow = 
  let buf =  Cstruct.sub (OS.Io_page.to_cstruct (OS.Io_page.get ())) 0 14 in 
  let _ = Cstruct.LE.set_uint32 buf 0 state.tx_target in 
  let _ = 
    match flow with
        | TCP -> Cstruct.LE.set_uint16 buf 4 6 
        | UDP ((Constant delay), size) -> 
            Cstruct.LE.set_uint16 buf 4 17;
            Cstruct.LE.set_uint16 buf 6 1;  
            Cstruct.LE.set_uint32 buf 8 (Int32.of_float (delay *. 1000000.0));
            Cstruct.LE.set_uint16 buf 12 size 
        | UDP ((Exp delay), size) -> 
            Cstruct.LE.set_uint16 buf 4 17;
            Cstruct.LE.set_uint16 buf 6 2;  
            Cstruct.LE.set_uint32 buf 8 (Int32.of_float (delay *. 1000000.0));
            Cstruct.LE.set_uint16 buf 12 size
        | UDP _ -> failwith "pttcp doesn't support pareto arrival models at the moment" 
  in buf 

let request_data state st mgr src_port dst  = 
  let buf = marshal_flow_request state st.flow in 
  lwt _  = write_and_flush mgr src_port dst buf in 
  let recv_data state u _ recv =
    let _ = update_rx_stat state (Int32.of_int (Cstruct.len recv)) in
    let _ = 
      try 
        if (state.tx_target <= state.rx_rcvd) then Lwt.wakeup u () 
      with exn -> (eprintf "%s\n%!" (Printexc.to_string exn))
    in
      return ()
  in
  let (t, u) = Lwt.task () in 
  let th = Net.Datagram.UDPv4.recv mgr (None, src_port) (recv_data state u) in 
  lwt _ = th <?> t in
  let _ = printf "thread returned for port %d\n%!" src_port in 
  let _ = Lwt.cancel th in 
    return ()

let simple_client st bytes state mgr src_port dhost dst_port = 
  try_lwt
    let _ = update_tx_target state bytes in 
    lwt _ = request_data state st mgr src_port (dhost, dst_port) in 
    let _ = if st.verbose then 
      eprintf "%03.6f: Finished with %d after %ld bytes (%ld pkts).\n%fs = %.4f b/s \n%!" 
        (OS.Clock.time ()) state.client_id state.rx_rcvd state.rx_pkts 
        (state.rx_stop -. state.rx_start) 
        ((Int32.to_float state.rx_rcvd) /. (state.rx_stop -. state.rx_start) )
    in 
      return ()
  with exn -> 
    return (eprintf "%03.6f: simple_client error: %s\n%!" (OS.Clock.time ())
      (Printexc.to_string exn))

let surge_client st interpage objperpage interobj objsize state mgr src_port dhost dst_port = 
  try_lwt
    match state.objperpage with
      | c when (c < 1.0) -> 
          let _ = state.objperpage <-  (get_sample objperpage) in
          OS.Time.sleep (get_sample interpage) 
      | c -> 
          lwt _ = OS.Time.sleep (get_sample interobj) in 
          let _ = update_tx_target state (Int32.of_float (get_sample objsize)) in
          lwt _ = request_data state st mgr src_port (dhost, dst_port) in
          let _ = state.objperpage <- state.objperpage -. 1.0 in
          let _ = if (st.verbose) then 
            eprintf "%03.6f: Finished with %d after %ld bytes (%ld pkts). %fs = %.4f b/s \n%!" 
              (OS.Clock.time ()) state.client_id  state.rx_rcvd state.rx_pkts
              (state.rx_stop -. state.rx_start) 
              ((Int32.to_float state.rx_rcvd) /. (state.rx_stop -. state.rx_start) )
          in 
            return ()
  with exn -> 
    return (eprintf "%03.6f: surge_client error: %s\n %s\n%!" (OS.Clock.time ()) 
      (Printexc.to_string exn) (Printexc.get_backtrace ()) ) 

let generate_traffic mgr mode model pkt_sz verbose = 
  let flow = UDP(model, pkt_sz) in 
  let st = init_pttcp_state_t mode flow verbose in
    match mode with 
(*      | Simple_rx(num_ports, base_port) -> 
          simple_rx st num_ports base_port 
      | Simple_tx(num_conn, bytes, dhost, num_ports, base_port) -> 
          simple_tx st num_conn bytes dhost num_ports base_port *)
      | Srv (num_ports, base_port ) -> 
          create_listeners mgr st num_ports base_port simple_server 
      | Simple_clt(num_conn, bytes, dhost, num_ports, base_port) -> 
          create_connectors mgr st dhost num_ports base_port num_conn
            false (simple_client st bytes)
      | Cts_ctl (num_conn, bytes, dhost, num_ports, base_port) -> 
          create_connectors mgr st dhost num_ports base_port num_conn 
            true (simple_client st bytes)
      | Surge_client (num_conn, dhost, num_ports, base_port, 
        interpage, objperpage, interobj, objsize) ->
          create_connectors mgr st dhost num_ports base_port num_conn 
            true (surge_client st interpage objperpage interobj objsize)
      | _ -> return (eprintf "Not Implemented mechanism\n%!")

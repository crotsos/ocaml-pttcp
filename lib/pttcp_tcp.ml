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

(*
 *
 * Surge client communicates with the server mode of pttcp
 * 
 * The objective is to partially replicate the behaviour of the SURGE
 * web-server tester of
 * 
 * P. Barford and M.E. Crovella, "Generating representative wb workloads
 * for network and server performance evaluation", In Proceedings of
 * Performance '98/ ACM Sigmetrics '98 pages 151-160, 1998
 * 
 * as used in 
 * 
 * Feldman A., et.al, "Dynamics of IP Traffic: A study of the role of
 * variability and the impact of control", SIGCOMM'99, pp 301-313.
 * 
 * This client is easiest thought of as a four stage Marokv chain.
 * 
 * the four stages are
 * 
 * -- interpage: time between consequtive pages downloaded in one session
 * 
 * -- objperpage: number of objects within a web page; all such objects
 *         are retrieved from the server before waiting for another page.
 * 
 * -- interobj: time between retriving each object on a single page.
 * 
 * -- objsize: size of an object in >BYTES<
 * 
 * each option above takes a distribution and the distribution
 * arguments....
 * 
 * constant <constant>
 * exponent <mean>
 * pareto   <mean> <shape>
 * 
 * allowing you to specify a moderatly complex Markox chain with
 * differing distributions and differing probabilities for each transtion
 * stage.
 * 
 * The sessions, once running are assumed to contain an `infinite' number
 * of pages, (or until the runtime is complete.)
 * 
 * Currently it does not calculate inter-session time or
 * pages-per-session, as these are considered the responsibility of a
 * test-rig (and can be set as the run_time, etc.)
 * 
 * When a session is opened the first object of the page is transmitted;
 * there is no random starting points displacing multiple connections in
 * one surge client.
 * 
 * 5 examples were used in the Feldman paper, these and their respective
 * parameter sets are given below.
 * 
 * 
 * -- objsize: size of an object in >BYTES<
 * 
 * each option above takes a distribution and the distribution
 * arguments....
 * 
 * constant <constant>
 * exponent <mean>
 * pareto   <mean> <shape>
 * 
 * allowing you to specify a moderatly complex Markox chain with
 * differing distributions and differing probabilities for each transtion
 * stage.
 * 
 * The sessions, once running are assumed to contain an `infinite' number
 * of pages, (or until the runtime is complete.)
 * 
 * Currently it does not calculate inter-session time or
 * pages-per-session, as these are considered the responsibility of a
 * test-rig (and can be set as the run_time, etc.)
 * 
 * When a session is opened the first object of the page is transmitted;
 * there is no random starting points displacing multiple connections in
 * one surge client.
 * 
 * 5 examples were used in the Feldman paper, these and their respective
 * parameter sets are given below.
 * 
 * Pareto 1 :
 * -- interpage pareto 50 2 \
 * -- objperpage pareto 4 1.2 \
 * -- interobj pareto 0.5 1.5 \
 * -- objsize pareto 12000 1.2
 * 
 * Pareto 2 :
 * -- interpage pareto 10 2 \
 * -- objperpage pareto 3 1.5 \
 * -- interobj pareto 0.5 1.5 \
 * -- objsize pareto 12000 1.2
 * 
 * Exp 1    :
 * -- interpage pareto 25 2 \
 * -- objperpage constant 1 \
 * -- interobj constant 0 \
 * -- objsize exponent 12000 
 * 
 * Exp 2    :
 * -- interpage exponent 10 \
 * -- objperpage constant 1 \
 * -- interobj constant 0 \
 * -- objsize exponent 12000
 * 
 * Constant :
 * -- interpage constant 10 \
 * -- objperpage constant 1 \
 * -- interobj constant 0 \
 * -- objsize constant 1e6
 * 
 **)

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
  | Exp(m) -> (log (Random.float max_float)) *. m
  | Pareto(m, s) -> 
      let v = Random.float max_float in
      let d = v ** (1.0/.s) in 
        m *. ( 1.0 /. d)

type traffic_model = 
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

type pttcp_t = {
  mutable states: state_t list;
  mutable finished: state_t list;
  verbose : bool;
  mode : traffic_model;
  mutable max_id : int;}

let init_pttcp_state_t mode verbose = 
  { states=[]; finished=[]; mode; max_id=0; verbose;}

let add_pttcp_state st src_port dst_ip dst_port = 
   let client_id = st.max_id in 
   let state = init_channel_state_t 
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
        let rate = ((Int32.to_float s.rx_rcvd_cpt) *. 8.0) /. 60000.0 in 
        let _ = s.rx_rcvd_cpt <- 0l in
        let _ = printf "+%.4fkbps " rate in 
          r) st.states 0l in
    let progress_count = 
      List.length (List.filter (fun s -> (s.rx_rcvd_cpt > 0l)) st.states) in 
    let active_count = List.length st.states in
    let finished_bytes = List.fold_right
      (fun s b ->
        let r = Int32.add s.rx_rcvd_cpt b in 
        let rate = ((Int32.to_float s.rx_rcvd_cpt) *. 8.0) /. 60000.0 in 
        let _ = if (st.verbose) then printf "-%.4fkbps " rate in 
          r) st.finished 0l in
    let finished_count = List.length st.finished in 
    let _ = st.finished <- [] in 
    let rate = ((Int32.to_float (Int32.add total_bytes finished_bytes)) *. 8.0) /. 60000000.0 in 
    let _ = printf "\n%d streams active, %d made progress, %d finished: tot = %ld, tot Mb/s = %.2f\n%!"
              active_count progress_count finished_count (Int32.add total_bytes finished_bytes) rate in
      return ()
  done

let print_pttcp_state_tx st = 
  while_lwt true do 
    lwt _ = OS.Time.sleep 1.0 in
    let total_bytes = List.fold_right
      (fun s b ->
        let r = Int32.add s.tx_sent_cpt b in
        let rate = ((Int32.to_float s.tx_sent_cpt) *. 8.0) /. 60000.0 in 
        let _ = s.tx_sent_cpt <- 0l in
        let _ = printf "+%.4fkbps " rate in 
          r) st.states 0l in
    let progress_count = 
      List.length (List.filter (fun s -> (s.tx_sent_cpt > 0l)) st.states) in 
    let active_count = List.length st.states in
    let finished_bytes = List.fold_right
      (fun s b ->
        let r = Int32.add s.tx_sent_cpt b in 
        let rate = ((Int32.to_float s.tx_sent_cpt) *. 8.0) /. 60000.0 in 
        let _ = if (st.verbose) then printf "-%.4fkbps " rate in 
          r) st.finished 0l in
    let finished_count = List.length st.finished in 
    let _ = st.finished <- [] in 
    let rate = ((Int32.to_float (Int32.add total_bytes finished_bytes)) *. 8.0) /. 60000000.0 in 
    let _ = printf "\n%d streams active, %d made progress, %d finished: tot = %ld, tot Mb/s = %.2f\n%!"
              active_count progress_count finished_count (Int32.add total_bytes finished_bytes) rate in
      return ()
  done 

(*
 * util methods
 * *)
let write_and_flush t buf = 
  let _ = Channel.write_buffer t buf in 
    Channel.flush t 

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
            let _ = printf "Openning port %d\n%!" port in
              Net.Channel.listen mgr (`TCPv4 ((None, port), (cb port) )) 
        with exn ->
          return (eprintf "%03.f: create_listeners error : %s\n%!" 
                    (OS.Clock.time ()) (Printexc.to_string exn))

      ) ports) <&> (print_pttcp_state_tx st)

let simple_server st src_port (dst_ip, dst_port) t = 
  let state = add_pttcp_state st src_port dst_ip dst_port in 
  try_lwt
    while_lwt true do 
      lwt buf = Channel.read_some ~len:4 t in
      let _ = printf "Read %d bytes\n%!" (Cstruct.len buf) in 
      let tx_len = Cstruct.LE.get_uint32 buf 0 in  
      let _ = printf "Need to send %ld bytes...\n%!" tx_len in 
      let _ = update_tx_target state tx_len in
      let rec send_data state t = function 
        | 0l -> return ()
        | len when (len > 1460l) -> 
            let buf = (Cstruct.sub (OS.Io_page.get ()) 0 1460) in 
            lwt _ = write_and_flush t buf in
            let _ = update_tx_stat state 1460l in 
              send_data state t (Int32.sub len 1460l)
        | len ->
            let buf = Cstruct.sub (OS.Io_page.get ()) 0 (Int32.to_int len) in 
            lwt _ = write_and_flush t buf in 
            let _ = update_tx_stat state len in
              return 
                (eprintf 
                   "%03.6f: flow %d - finished %ld bytes (%ld pkts)\n%!"
                   (OS.Clock.time ()) state.client_id state.tx_target 
                   state.tx_pkts)
      in 
      lwt _ = send_data state t tx_len in
        return (del_pttcp_state st state)
    done
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
              Net.Channel.connect mgr (`TCPv4 (None, (dhost, port), 
                                               (cb state dhost port)) )
            done 
          in
          let _ = del_pttcp_state st state in 
            return ()
      ) ports) <&> (print_pttcp_state_rx st)

let request_data st state t = 
  let buf =  Cstruct.sub (OS.Io_page.get ()) 0 4 in 
  let _ = Cstruct.LE.set_uint32 buf 0 state.tx_target in 
  lwt _  = write_and_flush t buf in 
    while_lwt (state.tx_target > state.rx_rcvd) do
      lwt recv = Channel.read_some t in 
      let _ = update_rx_stat state (Int32.of_int (Cstruct.len recv)) in
      return ()
    done

let simple_client st bytes state dhost dst_port t = 
  try_lwt
    let _ = update_tx_target state bytes in 
    lwt _ = request_data st state t in 
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

let surge_client st interpage objperpage interobj objsize state dhost dst_port t = 
  try_lwt
    match state.objperpage with
      | c when (c < 1.0) -> 
          let _ = state.objperpage <-  (get_sample objperpage) in
          OS.Time.sleep (get_sample interpage) 
      | c -> 
          lwt _ = OS.Time.sleep (get_sample interobj) in 
          let _ = update_tx_target state (Int32.of_float (get_sample objsize)) in
          lwt _ = request_data st state t in
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

let generate_traffic mgr mode verbose = 
  let st = init_pttcp_state_t mode verbose in 
    match mode with 
(*      | Simple_rx(num_ports, base_port) -> 
          simple_rx st num_ports base_port 
      | Simple_tx(num_conn, bytes, dhost, num_ports, base_port) -> 
          simple_tx st num_conn bytes dhost num_ports base_port *)
      | Srv (num_ports, base_port ) -> 
          create_listeners mgr st num_ports base_port (simple_server st)
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

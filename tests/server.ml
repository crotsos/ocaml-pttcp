open Lwt
open Net
open Pttcp.Pttcp_tcp

let run_client () = 
    Net.Manager.create (
      fun mgr interface id ->
(*        let dst_ip = Ipaddr.V4.make 10l 0l 1l 2l in *)
        let st = init_pttcp_state_t (Trace_server 6666) true in 
        generate_traffic mgr st
        (*        generate_traffic mgr (Srv(1, 6666)) true *)
    )

let _ = 
  OS.Main.run (run_client ())

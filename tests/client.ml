open Lwt
open Net
open Pttcp.Pttcp_tcp

let run_client () = 
    Net.Manager.create (
      fun mgr interface id ->
        let dst_ip = Ipaddr.V4.make 128l 232l 33l 59l in
        let _ = Printf.printf "trying to connect to the local server\n%!" in
        let model = Trace_client([dst_ip], 6666, 3, (Constant 1e6), (Pareto(7.5, 2.0))) in 
        let st = init_pttcp_state_t model true in 
        lwt _ = generate_traffic mgr st <?> (OS.Time.sleep 20.0) in

        let _ = 
          List.iter (
            fun s ->
              Printf.eprintf "%.06f: %d send %ld bytes in %.06f\n"
              s.end_ts s.req_id s.size (s.end_ts -. s.beg_ts)

          ) (stats st) in 
        return ()
        (*let model = Cts_ctl(1, 1000000l, dst_ip, 1, 6666) in
        generate_traffic mgr model true *)
    )

let _ = 
  OS.Main.run (run_client ())

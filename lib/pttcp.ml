open Printf
open Lwt
open Pttcp_tcp

let ip =
  let open Net.Nettypes in
  ( ipv4_addr_of_tuple (10l,0l,0l,2l),
    ipv4_addr_of_tuple (255l,255l,255l,0l),
   [ipv4_addr_of_tuple (10l,0l,0l,1l)]
  )

let main () =
  Net.Manager.create (fun mgr interface id ->
    Net.Manager.configure interface (`IPv4 ip) >>
    generate_traffic mgr 
(*     (Srv(10, 10000)) true >>  *)
(*       (Simple_clt(5, 20000l, Net.Nettypes.ipv4_addr_of_tuple (127l,0l,0l,1l),
 *       10, 10000)) true >>   *)
      (Cts_ctl(5, 20000l, Net.Nettypes.ipv4_addr_of_tuple (127l,0l,0l,1l), 10, 10000)) true >>  
    return (printf "Pttcp working \n%!")
  )

open Printf
open Lwt

let ip =
  let open Net.Nettypes in
  ( ipv4_addr_of_tuple (10l,0l,0l,2l),
    ipv4_addr_of_tuple (255l,255l,255l,0l),
   [ipv4_addr_of_tuple (10l,0l,0l,1l)]
  )

let main () =
  Net.Manager.create (fun mgr interface id ->
    Net.Manager.configure interface (`IPv4 ip) >>
    return (printf "Pttcp working \n%!")
  )

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

type model = 
  | Constant of float
  | Exp of float 
  | Pareto of float * float

type traffic_model = 
  | Simple_rx of int * int
  | Simple_tx of int * int32 * Ipaddr.V4.t * int * int
  | Srv of int * int
  | Simple_clt of int * int32 * Ipaddr.V4.t * int * int
  | Cts_ctl of int * int32 * Ipaddr.V4.t * int * int
  | Surge_client of int * Ipaddr.V4.t * int * int * model * model * model *model 
  | Trace_server of int
  | Trace_client of Ipaddr.V4.t list * int * int * model * model 

type stats = {
  beg_ts: float;
  end_ts: float;
  size: int32;
  req_id: int; 
}

type pttcp_t 

val init_pttcp_state_t: traffic_model -> bool -> pttcp_t
val generate_traffic: Net.Manager.t -> pttcp_t -> unit Lwt.t
val stats: pttcp_t -> stats list

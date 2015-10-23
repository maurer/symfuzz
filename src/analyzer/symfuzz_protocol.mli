open Protocol
open Misc

type protocol_t =
  | Ping of int
  | Pong of int
  | IVReq (* input vector request *)
  | IVResponse of Libinput_type.input_vector (* input vector response*)
  | OptionReq
  | OptionResponse of Options.t
  | Register of Libinput_type.input_vector
  | Result of int
  | ReqResult
  | Response
  | NoResponse

val recv : in_channel -> protocol_t
val send : out_channel -> protocol_t -> unit

val wait_for_ping : in_channel -> out_channel -> unit
val ping_check : in_channel -> out_channel -> unit
val client_prepare : Unix.file_descr -> in_channel * out_channel

val push_vector :
     out_channel
  -> Libinput_type.input_vector
  -> Libinput_type.input_vector

val pull_result :
     in_channel
  -> out_channel
  -> int


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

module ProcessProtocol = ProtocolMake(struct

    type t = protocol_t

    let version = 1

  end)

module ProcessCommunicate = Communicate (ProcessProtocol)

let recv = ProcessCommunicate.recv
let send = ProcessCommunicate.send

let wait_for_ping ic oc =
  match recv ic with
    | Ping v ->
        assert (v = ProcessProtocol.version);
        send oc (Pong ProcessProtocol.version)
    | _ -> raise ProtocolError

let ping_check ic oc =
  let () = send oc (Ping ProcessProtocol.version) in
  match recv ic with
    | Pong v -> assert (v = ProcessProtocol.version)
    | _ -> raise Not_found

let client_prepare sock =
  let ic = Unix.in_channel_of_descr sock in
  let oc = Unix.out_channel_of_descr sock in
  let () = ping_check ic oc in
  ic, oc

let push_vector oc vector =
  send oc (Register vector);
  vector

let pull_result ic oc =
  send oc ReqResult;
  match recv ic with
    | Result r -> r
    | _ -> raise ProtocolError


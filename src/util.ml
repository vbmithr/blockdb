let set_loglevel vs =
  let level = match List.length vs with
    | 0 -> Lwt_log.Notice
    | 1 -> Info
    | _ -> Debug in
  Lwt_log.add_rule "*" level

let is_verbose vs = List.length vs > 0
let is_debug vs = List.length vs > 1

module Cmdliner = struct
  module Conv = struct
    open Caml.Format
    let hex =
      (fun str ->
         try `Ok (Hex.to_string (`Hex str))
         with _ ->`Error (sprintf "Hex expected, got %s" str)),
      (fun ppf hex ->
         let `Hex hex_str = Hex.of_string hex in
         fprintf ppf "%s" hex_str)

    let tezos_addr =
      (fun str ->
         match Base58.Tezos.of_string str with
         | Some addr -> `Ok addr
         | None -> `Error (sprintf "Tezos address expected, got %s" str)),
      Base58.Tezos.pp

    let payment_addr =
      (fun str ->
         match Base58.Bitcoin.of_string str with
         | Some addr -> `Ok addr
         | None -> `Error (sprintf "Bitcoin multisig address expected, got %s" str)),
      Base58.Bitcoin.pp
  end

  open Cmdliner
  let loglevel =
    let doc = "Print more debug messages. Can be repeated." in
    Arg.(value & flag_all & info ["v"] ~doc)

  let testnet =
    let doc = "Use Bitcoin testnet." in
    Arg.(value & flag & info ["t" ; "testnet"] ~doc)

  let json =
    let doc = "Output in JSON format." in
    Arg.(value & flag & info ["j" ; "json"] ~doc)
end

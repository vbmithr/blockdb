open Lwt.Infix
open Cmdliner

open Util
open Util.Cmdliner
open Blockexplorer
open Blockexplorer_lwt
open Libbitcoin

let tn = ref false

let process_block hash =
  let `Hex hash_hex = Hash.Hash32.to_hex hash in
  begin rawblock ~testnet:!tn (`Hex hash_hex) >>= function
    | Error err ->
      Lwt_log.error (Http.string_of_error err) >>= fun () ->
      Lwt.fail Exit
    | Ok rawblock ->
      Lwt_log.debug_f "Downloaded rawblock hash %s" hash_hex >>= fun () ->
      Lwt.return rawblock
  end >>= fun rawblock ->
  let { Block.header ; transactions } = Block.of_bytes_exn rawblock in
  Lwt_log.debug_f "Processed block %s" (Hash.Hash32.show header.hash) >>= fun () ->
  (* TODO: Store in DB. *)
  Lwt.return header.prev_block_hash

let rec process_n_blocks n hash =
  process_block hash >>=
  process_n_blocks (pred n)

let build from n =
  begin network_status ~testnet:!tn () >>= function
    | Error err ->
      Lwt_log.error (Http.string_of_error err) >>= fun () ->
      Lwt.fail Exit
    | Ok { blocks } ->
      Lwt_log.debug_f "Found latest block height %d" blocks >>= fun () ->
      Lwt.return blocks
  end >>= fun blocks ->
  begin hash_of_block_index ~testnet:!tn (blocks - from) >>= function
    | Error err ->
      Lwt_log.error (Http.string_of_error err) >>= fun () ->
      Lwt.fail Exit
    | Ok (`Hex hash as hash_hex) ->
      Lwt_log.debug_f
        "Found block hash for height %d: %s" (blocks - from) hash >>= fun () ->
      Lwt.return (Hash.Hash32.of_hex_exn hash_hex)
  end >>=
  process_n_blocks n

let build loglevel testnet from n =
  set_loglevel loglevel ;
  if testnet then tn := true ;
  Lwt_main.run (build from n)

let build =
  let doc = "Build the database." in
  let from =
    let doc = "Starting point (negative index)." in
    Arg.(value & (opt int 6) & info ["f" ; "from" ] ~docv:"INT" ~doc) in
  let n =
    let doc = "Number of blocks to process." in
    Arg.(value & (opt int 10) & info ["n" ] ~docv:"INT" ~doc) in
  Term.(const build $ loglevel $ testnet $ from $ n),
  Term.info ~doc "build"

let default_cmd =
  let doc = "Crowdsale Block API." in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info ~doc "blockdb"

let cmds = [
  build ;
]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> Caml.exit 1
  | #Term.result -> Caml.exit 0


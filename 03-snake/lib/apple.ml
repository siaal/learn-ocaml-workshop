open! Base

type t = { location : Position.t } [@@deriving sexp_of]

let location t = t.location

(* DONE: Implement [create].

   Make sure to inspect the mli to understand the signature of[create]. [create]
   will take in the height and width of the board area, as well as a list of
   locations where the apple cannot be generated, and create a [t] with a random
   location on the board.

   Hint:
   - You can generate a random int up to [bound] via [Random.int bound].
   - You can pick a random element out of a list using [List.random_element_exn list].
*)
let create ~height ~width ~invalid_locations =
  let valid_positions =
    List.range 0 height
    |> List.map ~f:(fun row ->
      List.map (List.range 0 width) ~f:(fun col -> { Position.row; col }))
    |> List.concat
    |> List.filter ~f:(fun e ->
      not (List.mem invalid_locations e ~equal:[%compare.equal: Position.t]))
  in
  match valid_positions with
  | [] -> None
  | _ -> Some { location = List.random_element_exn valid_positions }
;;

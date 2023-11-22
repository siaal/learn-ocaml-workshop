open! Base

type t =
  { (* [direction] represents the orientation of the snake's head. *)
    direction : Direction.t
  ; (* [extensions_remaining] represents how many more times we should extend the
       snake. *)
    extensions_remaining : int
  ; (* [locations] represents the current set of squares that the snake
       occupies. The first element of the list is the head of the snake. We hold
       as an invariant that [locations] is always non-empty. *)
    locations : Position.t list
  }
[@@deriving sexp_of]

(* DONE: Implement [create].

   Note that at the beginning of the game, the snake will not need to grow at all, so
   [extensions_remaining] should be initialized to 0. *)
let create ~length =
  { direction = Right
  ; extensions_remaining = 0
  ; locations =
      List.init length ~f:(fun idx -> { Position.row = 0; col = idx }) |> List.rev
  }
;;

(* DONE: Implement [grow_over_next_steps].

   Read over the documentation of this function in the mli.

   Notice that this function should not actually grow the snake, but only record that we
   should grow the snake one block for the next [by_how_much] squares. *)
let grow_over_next_steps t by_how_much =
  { t with extensions_remaining = t.extensions_remaining + by_how_much }
;;

(* DONE: Implement [locations]. *)
let locations t = t.locations

(* DONE: Implement [head_location]. *)
let head_location t = List.hd_exn t.locations

(* DONE: Implement [set_direction]. *)
let set_direction t direction = { t with direction }

(* DONE: Implement [step].

   Read over the documentation of this function in the mli.

   [step] should:
   - move the snake forward one block, growing it and updating [t.locations] if necessary
   - check for self collisions *)
let remove_last lst =
  match List.rev lst with
  | [] -> []
  | _ :: tl -> List.rev tl
;;

let step ({ direction; extensions_remaining; locations } as t) =
  let locs, exts =
    if extensions_remaining > 0
    then locations, extensions_remaining - 1
    else remove_last locations, extensions_remaining
  in
  let new_pos = Direction.next_position direction (head_location t) in
  match List.mem locs new_pos ~equal:[%compare.equal: Position.t] with
  | true -> None
  | false -> Some { t with locations = new_pos :: locs; extensions_remaining = exts }
;;

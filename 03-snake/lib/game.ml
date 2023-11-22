open! Base

type t =
  { mutable snake : Snake.t
  ; mutable apple : Apple.t
  ; mutable game_state : Game_state.t
  ; height : int
  ; width : int
  ; amount_to_grow : int
  }
[@@deriving sexp_of]

(* DONE: Implement [in_bounds]. *)
let in_bounds { width; height; _ } { Position.col; row } =
  0 <= col && col < width && 0 <= row && row < height
;;

(* DONE: Implement [create].

   Make sure that the game returned by [create] is in a valid state. In particular, we
   should fail with the message "unable to create initial apple" if [Apple.create] is
   unsuccessful, and "unable to create initial snake" if the initial snake is invalid
   (i.e. goes off the board). *)
let create ~height ~width ~initial_snake_length ~amount_to_grow =
  let snake = Snake.create ~length:initial_snake_length in
  let apple = Apple.create ~width ~height ~invalid_locations:(Snake.locations snake) in
  match apple with
  | None -> failwith "unable to create initial apple"
  | Some apple ->
    let t = { snake; height; width; amount_to_grow; game_state = In_progress; apple } in
    if List.exists ~f:(fun p -> not (in_bounds t p)) (Snake.locations snake)
    then failwith "unable to create initial snake"
    else t
;;

let snake t = t.snake
let apple t = t.apple
let game_state t = t.game_state

(* DONE: Implement [set_direction]. *)
let set_direction t direction = t.snake <- Snake.set_direction t.snake direction

(* DONE: Implement [step].

   [step] should:
   - move the snake forward one square
   - check for collisions (end the game with "Wall collision" or "Self collision")
   - if necessary:
     -- consume apple
     -- if apple cannot be regenerated, win game; otherwise, grow the snake *)

let step t =
  let do_snake_step () =
    match Snake.step t.snake with
    | None -> t.game_state <- Game_over "Self collision"
    | Some snake ->
      t.snake <- snake;
      let head = Snake.head_location snake in
      if not (in_bounds t head) then t.game_state <- Game_over "Wall collision"
  in
  let do_apple () =
    if [%compare.equal: Position.t] (Snake.head_location t.snake) (Apple.location t.apple)
    then (
      (* t.snake <- Snake.grow_over_next_steps t.snake t.amount_to_grow; *)
      let snake = Snake.grow_over_next_steps t.snake t.amount_to_grow in
      match
        Apple.create
          ~height:t.height
          ~width:t.width
          ~invalid_locations:(Snake.locations snake)
      with
      | None -> t.game_state <- Win
      | Some apple ->
        t.snake <- snake;
        t.apple <- apple)
    else ()
  in
  do_snake_step ();
  do_apple ();
  ()
;;

module For_testing = struct
  let create_apple_force_location_exn ~height ~width ~location =
    let invalid_locations =
      List.init height ~f:(fun row ->
        List.init width ~f:(fun col -> { Position.row; col }))
      |> List.concat
      |> List.filter ~f:(fun pos -> not ([%compare.equal: Position.t] location pos))
    in
    match Apple.create ~height ~width ~invalid_locations with
    | None -> failwith "[Apple.create] returned [None] when [Some _] was expected!"
    | Some apple -> apple
  ;;

  let create_apple_and_update_game_exn t ~apple_location =
    let apple =
      create_apple_force_location_exn
        ~height:t.height
        ~width:t.width
        ~location:apple_location
    in
    t.apple <- apple
  ;;

  let create_game_with_apple_exn
    ~height
    ~width
    ~initial_snake_length
    ~amount_to_grow
    ~apple_location
    =
    let t = create ~height ~width ~initial_snake_length ~amount_to_grow in
    create_apple_and_update_game_exn t ~apple_location;
    t
  ;;
end

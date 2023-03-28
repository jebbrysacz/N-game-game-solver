open Boardrep
open Boardmetric
open Printf

module type Board =
  sig
    type t

    val init         : int -> t
    val load         : int -> int list -> t
    val get_size     : t -> int
    val get          : t -> loc -> int
    val randomize    : t -> int -> t
    val compare      : t -> t -> int
    val eval         : t -> t -> int
    val next         : t -> t list
    val get_contents : t -> (loc * int) list
    val display      : t -> string
    val dump         : t -> string
    val interact     : t -> unit
    val check_board  : t -> bool
  end

module Make (B : BoardRep) (M : BoardMetric with type t = B.t) : Board =
  struct
    type t = B.t

    (* These functions just dispatch to the BoardRep equivalents. *)
    let init size     = B.init size
    let load size lst = B.load size lst
    let get_size b    = B.get_size b
    let get b         = B.get b

    let random_move b =
      let valid_moves =
        List.filter
          (fun m ->
             try
               let _ = B.make_move b m in true
             with B.Invalid_move ->
               false)
          [Up; Down; Left; Right]
      in
      let nmoves = List.length valid_moves in
      let rmove = List.nth valid_moves (Random.int nmoves) in
        B.make_move b rmove

    let rec randomize b n =
      if n = 0 then 
        b 
      else 
        randomize (random_move b) (n - 1)

    let compare = Stdlib.compare

    let eval = M.distance

    let next b =
      let try_move b m =
        try Some (B.make_move b m) with B.Invalid_move -> None
      in
        List.filter_map (try_move b) [Up; Down; Left; Right]

    let get_contents b =
      let size = B.get_size b in
      let rec iter r c lst =
        match () with
          | _ when r = size -> List.rev lst
          | _ when c = size -> iter (r + 1) 0 lst
          | _ -> iter r (c + 1) (((r, c), B.get b (r, c)) :: lst)
      in
        iter 0 0 []

    let display b =
      let size = B.get_size b in
      let rec iter r c s =
        match () with
          | _ when r = size -> s
          | _ when c = size -> iter (r + 1) 0 (s ^ "\n")
          | _ -> 
            let v = B.get b (r, c) in
            let s' = 
              if v = 0 then
                Printf.sprintf "  ."
              else
                Printf.sprintf " %2d" v 
            in
              iter r (c + 1) (s ^ s')
      in
        iter 0 0 ""

    let dump b =
      let size = B.get_size b in
      let rec iter first r c s =
        match () with
          | _ when r = size -> s
          | _ when c = size -> iter first (r + 1) 0 s
          | _ -> 
            let v = B.get b (r, c) in
            let s' =
              if first then 
                Printf.sprintf "%d" v
              else
                Printf.sprintf "; %d" v
            in
              iter false r (c + 1) (s ^ s')
      in
      (iter true 0 0 "[") ^ "]"

    let interact b =
      let rec get_input () =
        begin
          Printf.printf "Enter move (u,d,l,r,q) > %!";
          let move_string = read_line () in
            (* NOTE: We flip the direction of the moves because
             * the solver considers a move to be the direction
             * that the hole moves, whereas human solvers prefer
             * to consider a move to be the direction a piece
             * makes into the hole. *)
            match move_string with
              | "u" -> Down
              | "d" -> Up
              | "l" -> Right
              | "r" -> Left
              | "q" -> raise Exit
              | _ ->
                begin
                  Printf.printf 
                    "ERROR: only \"u\", \"d\", \"l\", \"r\", or \"q\" %!";
                  Printf.printf "inputs are accepted!\n%!";
                  get_input ()
                end
        end
      in
      let rec make_move b =
        let move = get_input () in
          try
            B.make_move b move
          with B.Invalid_move -> 
            begin
              Printf.printf "ERROR: invalid move\n%!";
              make_move b
            end
      in
      let rec iter b =
        let solved = init (B.get_size b) in
        begin
          Printf.printf "\n%s\n%!" (display b);
          if eval solved b = 0 then
            Printf.printf "SOLVED!\n\n%!"
          else
            iter (make_move b)
        end
      in
        try iter b with Exit -> ()

    (* Generate a list of integers from 0 to n-1. *)
    let make_vals n =
      let rec iter i lst = 
        if i < 0 then
          lst
        else
          iter (i - 1) (i :: lst)
      in
        iter (n - 1) []

    (* Generate a list of locations from (0, 0) to (n-1, n-1). *)
    let make_locs n =
      let rec iter i j lst =
        match (i, j) with
          | _ when i >= n -> List.rev lst
          | _ when j >= n -> iter (i + 1) 0 lst
          | _ -> iter i (j + 1) ((i, j) :: lst)
      in
        iter 0 0 []

    (* Check that a board is in a valid state.
     * The board should have the correct number of elements.
     * All locations from (0, 0) to (size-1, size-1) should be represented.
     * All values from 0 to size-1 should be represented.
     * (This implies no duplicated values.) *)
    let check_board b =
      let size = B.get_size b in
      let len = size * size in
      let contents = get_contents b in
      let contents_len = List.length contents in
        if contents_len <> len then
          begin
            printf "FAIL: board has %d elements but should have %d elements\n%!"
              contents_len len;
            false
          end
        else
          let vals = List.map snd contents in
          let svals = List.sort compare vals in
          let cvals = make_vals (size * size) in
            if svals <> cvals then
              begin
                printf "FAIL: board has incorrect values\n%!";
                false
              end
            else
              let locs = List.map fst contents in
              let slocs = List.sort compare locs in
              let clocs = make_locs size in
                if slocs <> clocs then
                  begin
                    printf "FAIL: board has incorrect locations\n%!";
                    false
                  end
                else
                  true

  end


open Boardrep
open Board
open Boardmetric
open Astar
open Printf

module type Solver =
  sig
    module B : Board

    val init           : int -> int -> B.t
    val load           : int -> int list -> B.t
    val solve          : bool -> B.t -> B.t list option
    val print          : B.t list -> unit
    val interact       : B.t -> unit
    val check_solution : B.t list -> bool
  end

module NPuzzle (BR : BoardRep) (M : BoardMetric with type t = BR.t) : Solver =
  struct
    module B = Make(BR)(M)   (* board module *)
    module A = AStar(B)      (* solver module *)

    let init side rand = B.randomize (B.init side) rand

    let load size lst = B.load size lst

    let solve verbose start =
      let size = B.get_size start in
        begin
          if verbose then
            printf "Configuration: %s\n" (B.dump start);
          let goal = B.init size in
            try
              Some (A.solve goal start)
            with A.No_solution ->
              None
        end

    let print solution =
      let outfile = open_out "npuzzle.out" in
        begin
          List.iter 
            (fun b -> fprintf outfile "%s\n" (B.display b))
            solution;
          fprintf outfile "%d\n" (List.length solution - 1);
          close_out outfile;
        end

    let interact = B.interact

    (* -------------------------------------------------------------------- 
     * Checking that a solution is valid.
     * -------------------------------------------------------------------- *)

    (* Check that two locations are adjacent. *)
    let adjacent_locs (r1, c1) (r2, c2) =
      let rd = abs (r1 - r2) in
      let cd = abs (c1 - c2) in
        ((rd, cd) = (0, 1)) || ((rd, cd) = (1, 0))

    (* Check that exactly two locations are different.
       The inputs are (loc, value) lists representing
       the board contents.  
       Assumption: the locs are in sorted order. *)
    let get_diffs c1 c2 =
      let rec iter c1 c2 ds =
        match (c1, c2) with
          | ([], []) -> ds
          | (_,  []) -> failwith "get_diffs: unequal length lists"
          | ([],  _) -> failwith "get_diffs: unequal length lists"
          | (((r1, c1), v1) :: t1,
             ((r2, c2), v2) :: t2) ->
            if r1 <> r2 || c1 <> c2 then
              failwith "iter: incorrect row/column indices"
            else if v1 <> v2 then
              iter t1 t2 ((r1, c1) :: ds)
            else
              iter t1 t2 ds
      in
        iter c1 c2 []

    (* Check that board b1 can be converted to board b2 by making one move. *)
    let check_move b1 b2 =
      let contents1 = List.sort compare (B.get_contents b1) in
      let contents2 = List.sort compare (B.get_contents b2) in
      let diffs = get_diffs contents1 contents2 in
        (* Check that only two locations are different. *)
        match diffs with
          [loc1; loc2] ->
            (* Check that the two different locations are adjacent. *)
            if not (adjacent_locs loc1 loc2) then
              begin
                printf "BOARD1: %s\n" (B.dump b1);
                printf "BOARD2: %s\n" (B.dump b2);
                printf "FAIL: two locations that differ after one move ";
                printf "should be adjacent\n%!";
                false
              end
            else
              (* Check that the values at the two different locations
                 are swapped and that one of them is a zero. *)
              let v1a = B.get b1 loc1 in
              let v1b = B.get b2 loc1 in
              let v2a = B.get b1 loc2 in
              let v2b = B.get b2 loc2 in
                if v1a = v2b && v1b = v2a && (v1a = 0 || v1b = 0) then
                  true
                else
                  begin
                    printf "BOARD1: %s\n" (B.dump b1);
                    printf "BOARD2: %s\n" (B.dump b2);
                    printf "FAIL: two locations that differ after one move ";
                    printf "should contain same values, ";
                    printf "and one should be 0\n%!";
                    false
                  end
          | _ ->
            begin
              printf "BOARD1: %s\n" (B.dump b1);
              printf "BOARD2: %s\n" (B.dump b2);
              printf "FAIL: difference between boards one move apart ";
              printf "should be in exactly two locations\n%!";
              false
            end

    (* Check that board b is in the solved state. *)
    let check_end b = 
      let size = B.get_size b in
      let goal = B.init size in
        if B.eval goal b = 0 then
          true
        else
          begin
            printf "BOARD: %s\n" (B.dump b);
            printf "FAIL: ending board is not in the solved state\n%!";
            false
          end

    let check_solution lst =
      (* Check that a solution (list of boards) is valid. *)
      let rec iter lst =
        match lst with
          | [] -> true
          | [b] -> B.check_board b && check_end b
          | b1 :: ((b2 :: _) as t) -> 
              B.check_board b1 && 
              B.check_board b2 && 
              check_move b1 b2 &&
              iter t
      in
        if lst = [] then  (* no boards is not a valid solution *)
          false
        else
          iter lst
  end


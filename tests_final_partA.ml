(* CS 4, Winter 2023: Tests for final exam. *)

open OUnit2
open Printf

open Boardrep
open Boardmetric
open Npuzzle

(*** Generated modules. ***)

(* Metrics. *)
module Mhm = Hamming(MapRep)
module Mmm = Manhattan(MapRep)
module Mha = Hamming(ArrayRep)
module Mma = Manhattan(ArrayRep)

(* Boards. *)
module Bhm = Board.Make(MapRep)(Mhm)
module Bmm = Board.Make(MapRep)(Mmm)
module Bha = Board.Make(ArrayRep)(Mha)
module Bma = Board.Make(ArrayRep)(Mma)

(* Puzzle solvers. *)
module Phm = NPuzzle(MapRep)(Mhm)
module Pmm = NPuzzle(MapRep)(Mmm)
module Pha = NPuzzle(ArrayRep)(Mha)
module Pma = NPuzzle(ArrayRep)(Mma)

(*** Utility functor. ***)

module Contents (B : BoardRep) =
  struct
    let get_contents b =
      let size = B.get_size b in
      let rec iter r c lst =
        match () with
          | _ when r = size -> List.rev lst
          | _ when c = size -> iter (r + 1) 0 lst
          | _ -> iter r (c + 1) (((r, c), B.get b (r, c)) :: lst)
      in
        iter 0 0 []

    let get_values b = List.map snd (get_contents b)

    (* This works for both contents lists and values lists. *)
    let is_equal b1 b2 =
      let b1' = List.sort compare b1 in
      let b2' = List.sort compare b2 in
        b1' = b2'

    (* Make a contents list from an ordered locs list and a list of values. *)
    let make_contents = List.combine
  end

module CM = Contents(MapRep)
module CA = Contents(ArrayRep)

(*** Utility data and generated data. ***)

let solved3 = [1; 2; 3; 4; 5; 6; 7; 8; 0]
let solved3_all =
  [((0, 0), 1); ((0, 1), 2); ((0, 2), 3);
   ((1, 0), 4); ((1, 1), 5); ((1, 2), 6);
   ((2, 0), 7); ((2, 1), 8); ((2, 2), 0)]

let solved4 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 0]
let solved4_all =
  [((0, 0), 1);  ((0, 1), 2);  ((0, 2), 3);  ((0, 3), 4);
   ((1, 0), 5);  ((1, 1), 6);  ((1, 2), 7);  ((1, 3), 8);
   ((2, 0), 9);  ((2, 1), 10); ((2, 2), 11); ((2, 3), 12);
   ((3, 0), 13); ((3, 1), 14); ((3, 2), 15); ((3, 3), 0)]

let solved5 = 
  [1; 2; 3; 4; 5; 
   6; 7; 8; 9; 10; 
   11; 12; 13; 14; 15;
   16; 17; 18; 19; 20;
   21; 22; 23; 24; 0]
let solved5_all =
  [((0, 0), 1);  ((0, 1), 2);  ((0, 2), 3);  ((0, 3), 4);  ((0, 4), 5);
   ((1, 0), 6);  ((1, 1), 7);  ((1, 2), 8);  ((1, 3), 9);  ((1, 4), 10);
   ((2, 0), 11); ((2, 1), 12); ((2, 2), 13); ((2, 3), 14); ((2, 4), 15);
   ((3, 0), 16); ((3, 1), 17); ((3, 2), 18); ((3, 3), 19); ((3, 4), 20);
   ((4, 0), 21); ((4, 1), 22); ((4, 2), 23); ((4, 3), 24); ((4, 4), 0)]

let hamming3 = [
  ([1; 2; 3; 4; 5; 6; 7; 8; 0], 0);  (* test #0 *)
  ([1; 2; 3; 4; 5; 6; 7; 0; 8], 1);
  ([1; 2; 3; 4; 5; 0; 7; 8; 6], 1);
  ([1; 2; 3; 4; 0; 6; 7; 5; 8], 2);
  ([1; 2; 3; 4; 6; 0; 7; 5; 8], 3);
  ([1; 2; 3; 4; 6; 8; 7; 5; 0], 3);
  ([4; 1; 3; 7; 2; 6; 5; 8; 0], 5);
  ([7; 1; 3; 0; 2; 6; 5; 4; 8], 6);
  ([1; 2; 6; 3; 8; 4; 5; 7; 0], 6);
  ([6; 3; 5; 2; 0; 4; 7; 8; 1], 6);
  ([8; 4; 5; 1; 2; 3; 7; 6; 0], 7);  (* test #10 *)
  ([2; 3; 5; 8; 0; 6; 1; 4; 7], 7);
  ([8; 4; 1; 6; 0; 3; 7; 2; 5], 7);
  ([6; 4; 7; 8; 5; 0; 3; 2; 1], 7);
  ([2; 1; 7; 6; 0; 5; 8; 3; 4], 8);
  ([5; 6; 0; 1; 8; 7; 2; 4; 3], 8);
  ([0; 8; 7; 1; 2; 4; 6; 3; 5], 8);
  ([0; 4; 7; 5; 6; 1; 2; 3; 8], 8);
  ([6; 8; 5; 2; 0; 1; 3; 4; 7], 8);
  ([8; 3; 4; 5; 0; 7; 2; 6; 1], 8);
]

let hamming4 = [
  ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 0], 0);  (* test #0 *)
  ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 0; 15], 1);
  ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 0; 13; 14; 15; 12], 1);
  ([1; 2; 4; 7; 5; 6; 0; 3; 9; 10; 11; 8; 13; 14; 15; 12], 5);
  ([1; 2; 3; 4; 5; 6; 11; 7; 9; 10; 15; 8; 13; 0; 14; 12], 6);
  ([0; 2; 3; 4; 1; 6; 7; 8; 5; 10; 11; 12; 9; 13; 14; 15], 6);
  ([1; 2; 3; 4; 6; 7; 8; 12; 5; 10; 9; 11; 13; 0; 14; 15], 9);
  ([1; 2; 0; 4; 5; 11; 3; 6; 9; 7; 15; 8; 13; 10; 12; 14], 9);
  ([0; 1; 2; 4; 5; 7; 3; 8; 10; 6; 11; 12; 9; 13; 14; 15], 10);
  ([1; 2; 3; 4; 9; 5; 10; 7; 11; 6; 15; 8; 13; 0; 14; 12], 10);
  ([5; 1; 2; 8; 7; 0; 4; 3; 9; 6; 10; 11; 13; 14; 15; 12], 11); (* test #10 *) 
  ([6; 1; 3; 4; 9; 5; 11; 7; 14; 10; 0; 8; 2; 13; 15; 12], 11);
  ([2; 5; 3; 6; 1; 8; 13; 4; 9; 15; 0; 12; 10; 14; 7; 11], 11);
  ([15; 2; 0; 6; 1; 10; 11; 5; 9; 14; 4; 12; 13; 8; 7; 3], 11);
  ([0; 3; 4; 2; 9; 1; 8; 7; 13; 10; 15; 12; 6; 14; 5; 11], 12);
  ([0; 5; 2; 4; 7; 1; 3; 8; 10; 13; 15; 12; 6; 9; 11; 14], 12);
  ([0; 1; 8; 6; 5; 3; 11; 4; 9; 7; 2; 15; 13; 10; 12; 14], 12);
  ([15; 2; 10; 3; 1; 5; 7; 12; 14; 6; 8; 11; 13; 4; 9; 0], 12);
  ([15; 5; 1; 4; 2; 6; 9; 8; 3; 12; 7; 11; 14; 0; 10; 13], 12);
  ([9; 11; 3; 10; 14; 4; 15; 1; 2; 5; 8; 12; 13; 0; 7; 6], 12);
  ([14; 10; 0; 15; 5; 13; 12; 4; 9; 6; 3; 7; 8; 1; 2; 11], 13); (* test #20 *)
  ([8; 2; 14; 9; 5; 10; 11; 7; 0; 3; 4; 1; 12; 6; 13; 15], 13);
  ([2; 10; 1; 4; 15; 3; 11; 7; 0; 13; 12; 8; 5; 9; 6; 14], 14);
  ([9; 3; 0; 4; 15; 14; 8; 11; 5; 2; 10; 1; 6; 13; 12; 7], 14);
  ([15; 1; 14; 11; 8; 4; 12; 13; 9; 5; 10; 3; 6; 0; 2; 7], 14);
  ([10; 2; 5; 9; 7; 12; 13; 14; 8; 3; 6; 4; 11; 0; 1; 15], 14);
  ([1; 14; 8; 6; 15; 0; 10; 12; 3; 4; 13; 2; 7; 11; 9; 5], 14);
  ([9; 3; 10; 7; 13; 5; 12; 2; 0; 15; 1; 8; 14; 6; 11; 4], 15);
  ([2; 6; 14; 7; 11; 9; 4; 3; 1; 8; 10; 5; 15; 0; 13; 12], 15);
  ([7; 9; 2; 8; 6; 1; 15; 11; 3; 5; 0; 4; 14; 12; 13; 10], 15);
  ([0; 1; 14; 2; 12; 5; 11; 7; 3; 6; 15; 9; 4; 10; 13; 8], 15); (* test #30 *)
  ([15; 14; 5; 2; 3; 0; 6; 12; 13; 11; 10; 1; 9; 4; 8; 7], 15);
  ([14; 8; 11; 1; 6; 0; 13; 10; 7; 9; 3; 4; 2; 15; 12; 5], 15);
  ([4; 13; 1; 15; 12; 7; 9; 2; 0; 11; 8; 6; 14; 5; 3; 10], 15);
  ([0; 10; 8; 14; 13; 5; 3; 1; 15; 7; 12; 6; 4; 11; 2; 9], 15);
  ([13; 11; 6; 14; 15; 3; 8; 1; 12; 9; 5; 10; 4; 0; 2; 7], 15);
  ([15; 14; 8; 12; 10; 11; 9; 13; 2; 6; 5; 1; 3; 7; 4; 0], 15);
]

let manhattan3 = [
  ([1; 2; 3; 4; 5; 6; 7; 8; 0], 0);  (* test #0 *)
  ([1; 2; 3; 4; 5; 6; 7; 0; 8], 1);
  ([1; 2; 3; 4; 5; 0; 7; 8; 6], 1);
  ([1; 2; 3; 4; 0; 6; 7; 5; 8], 2);
  ([1; 2; 3; 4; 6; 0; 7; 5; 8], 3);
  ([1; 2; 3; 4; 6; 8; 7; 5; 0], 4);
  ([4; 1; 3; 7; 2; 6; 5; 8; 0], 6);
  ([7; 1; 3; 0; 2; 6; 5; 4; 8], 9);
  ([1; 2; 6; 3; 8; 4; 5; 7; 0], 10);
  ([8; 4; 5; 1; 2; 3; 7; 6; 0], 12);
  ([2; 3; 5; 8; 0; 6; 1; 4; 7], 12); (* test #10 *)
  ([8; 4; 1; 6; 0; 3; 7; 2; 5], 14);
  ([6; 3; 5; 2; 0; 4; 7; 8; 1], 14);
  ([2; 1; 7; 6; 0; 5; 8; 3; 4], 16);
  ([5; 6; 0; 1; 8; 7; 2; 4; 3], 16);
  ([0; 8; 7; 1; 2; 4; 6; 3; 5], 18);
  ([0; 4; 7; 5; 6; 1; 2; 3; 8], 18);
  ([6; 8; 5; 2; 0; 1; 3; 4; 7], 20);
  ([8; 3; 4; 5; 0; 7; 2; 6; 1], 20);
  ([6; 4; 7; 8; 5; 0; 3; 2; 1], 21);
]

let manhattan4 = [
  ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 0], 0);  (* test #0 *)
  ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 0; 15], 1);
  ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 0; 13; 14; 15; 12], 1);
  ([1; 2; 3; 4; 5; 6; 11; 7; 9; 10; 15; 8; 13; 0; 14; 12], 6);
  ([0; 2; 3; 4; 1; 6; 7; 8; 5; 10; 11; 12; 9; 13; 14; 15], 6);
  ([1; 2; 4; 7; 5; 6; 0; 3; 9; 10; 11; 8; 13; 14; 15; 12], 7);
  ([0; 1; 2; 4; 5; 7; 3; 8; 10; 6; 11; 12; 9; 13; 14; 15], 10);
  ([1; 2; 3; 4; 6; 7; 8; 12; 5; 10; 9; 11; 13; 0; 14; 15], 10);
  ([1; 2; 3; 4; 9; 5; 10; 7; 11; 6; 15; 8; 13; 0; 14; 12], 12);
  ([5; 1; 2; 8; 7; 0; 4; 3; 9; 6; 10; 11; 13; 14; 15; 12], 14);
  ([1; 2; 0; 4; 5; 11; 3; 6; 9; 7; 15; 8; 13; 10; 12; 14], 14); (* test #10 *)
  ([6; 1; 3; 4; 9; 5; 11; 7; 14; 10; 0; 8; 2; 13; 15; 12], 16);
  ([0; 3; 4; 2; 9; 1; 8; 7; 13; 10; 15; 12; 6; 14; 5; 11], 20);
  ([0; 5; 2; 4; 7; 1; 3; 8; 10; 13; 15; 12; 6; 9; 11; 14], 20);
  ([2; 5; 3; 6; 1; 8; 13; 4; 9; 15; 0; 12; 10; 14; 7; 11], 22);
  ([0; 1; 8; 6; 5; 3; 11; 4; 9; 7; 2; 15; 13; 10; 12; 14], 22);
  ([15; 2; 10; 3; 1; 5; 7; 12; 14; 6; 8; 11; 13; 4; 9; 0], 26);
  ([2; 10; 1; 4; 15; 3; 11; 7; 0; 13; 12; 8; 5; 9; 6; 14], 26);
  ([15; 5; 1; 4; 2; 6; 9; 8; 3; 12; 7; 11; 14; 0; 10; 13], 28);
  ([15; 2; 0; 6; 1; 10; 11; 5; 9; 14; 4; 12; 13; 8; 7; 3], 28);
  ([9; 3; 0; 4; 15; 14; 8; 11; 5; 2; 10; 1; 6; 13; 12; 7], 30); (* test #20 *)
  ([9; 3; 10; 7; 13; 5; 12; 2; 0; 15; 1; 8; 14; 6; 11; 4], 30);
  ([2; 6; 14; 7; 11; 9; 4; 3; 1; 8; 10; 5; 15; 0; 13; 12], 32);
  ([7; 9; 2; 8; 6; 1; 15; 11; 3; 5; 0; 4; 14; 12; 13; 10], 32);
  ([0; 1; 14; 2; 12; 5; 11; 7; 3; 6; 15; 9; 4; 10; 13; 8], 34);
  ([9; 11; 3; 10; 14; 4; 15; 1; 2; 5; 8; 12; 13; 0; 7; 6], 34);
  ([14; 10; 0; 15; 5; 13; 12; 4; 9; 6; 3; 7; 8; 1; 2; 11], 36);
  ([8; 2; 14; 9; 5; 10; 11; 7; 0; 3; 4; 1; 12; 6; 13; 15], 36);
  ([15; 14; 5; 2; 3; 0; 6; 12; 13; 11; 10; 1; 9; 4; 8; 7], 38);
  ([14; 8; 11; 1; 6; 0; 13; 10; 7; 9; 3; 4; 2; 15; 12; 5], 40);
  ([4; 13; 1; 15; 12; 7; 9; 2; 0; 11; 8; 6; 14; 5; 3; 10], 40); (* test #30 *)
  ([0; 10; 8; 14; 13; 5; 3; 1; 15; 7; 12; 6; 4; 11; 2; 9], 42);
  ([15; 1; 14; 11; 8; 4; 12; 13; 9; 5; 10; 3; 6; 0; 2; 7], 42);
  ([10; 2; 5; 9; 7; 12; 13; 14; 8; 3; 6; 4; 11; 0; 1; 15], 44);
  ([1; 14; 8; 6; 15; 0; 10; 12; 3; 4; 13; 2; 7; 11; 9; 5], 44);
  ([13; 11; 6; 14; 15; 3; 8; 1; 12; 9; 5; 10; 4; 0; 2; 7], 46);
  ([15; 14; 8; 12; 10; 11; 9; 13; 2; 6; 5; 1; 3; 7; 4; 0], 48);
]

let boards3_locs =
  [(0, 0); (0, 1); (0, 2); (1, 0); (1, 1); (1, 2); (2, 0); (2, 1); (2, 2)]

let boards3 =
  [|
    [1; 2; 3; 4; 5; 6; 7; 8; 0];  (* 0:  0 moves *)
    [1; 2; 3; 4; 6; 8; 0; 7; 5];  (* 1:  6 moves *)
    [1; 3; 6; 7; 4; 8; 0; 2; 5];  (* 2: 10 moves *)
    [6; 1; 8; 3; 7; 2; 0; 4; 5];  (* 3: 18 moves *)
    [4; 3; 5; 1; 7; 2; 8; 6; 0];  (* 4: 20 moves *)
    [8; 3; 7; 6; 0; 5; 1; 4; 2];  (* 5: 26 moves *)
    [8; 6; 7; 2; 5; 4; 3; 0; 1];  (* 6: 31 moves *)
    [6; 4; 7; 8; 5; 0; 3; 2; 1];  (* 7: 31 moves *)
    [1; 2; 3; 4; 5; 6; 8; 7; 0];  (* 8: unsolvable *)
  |]

let bad_boards3 =
  [|
    [1; 2; 3; 4; 5; 6; 7; 0];        (* too few elements *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 0];  (* too many elements *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9];     (* no hole. *)
    [1; 2; 3; 4; 5; 6; 1; 8; 0];     (* all numbers not represented *)
  |]

let boards3_results =
  [|
    Some 0; Some 6; Some 10; Some 18; 
    Some 20; Some 26; Some 31; Some 31;
    None
  |]

let boards4_locs =
  [(0, 0); (0, 1); (0, 2); (0, 3);
   (1, 0); (1, 1); (1, 2); (1, 3);
   (2, 0); (2, 1); (2, 2); (2, 3);
   (3, 0); (3, 1); (3, 2); (3, 3)]

let boards4 =
  [|
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 0];  (* 0:  0 moves *)
    [1; 2; 3; 4; 6; 7; 11; 8; 5; 10; 0; 12; 9; 13; 14; 15];  (* 1:  8 moves *)
    [0; 1; 2; 4; 10; 5; 3; 8; 6; 15; 7; 11; 9; 13; 14; 12];  (* 2: 16 moves *)
    [2; 6; 1; 3; 5; 9; 7; 4; 13; 11; 0; 12; 10; 14; 15; 8];  (* 3: 26 moves *)
    [9; 5; 6; 4; 13; 3; 1; 11; 0; 10; 12; 7; 14; 2; 15; 8];  (* 4: 34 moves *)
    [5; 4; 0; 9; 12; 1; 7; 3; 13; 6; 10; 15; 11; 8; 14; 2];  (* 5: 48 moves *)
    [3; 8; 13; 1; 2; 15; 9; 5; 12; 4; 0; 7; 10; 14; 6; 11];  (* 6: 50 moves *)
    [3; 9; 8; 5; 2; 0; 15; 14; 4; 10; 11; 13; 1; 6; 12; 7];  (* 7: 52 moves *)
    [15; 14; 8; 12; 10; 11; 9; 13; 2; 6; 5; 1; 3; 7; 4; 0];  (* 8: 80 moves *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 15; 14; 0];  (* 9: unsolvable *)
  |]

let bad_boards4 =
  [|
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 0];         (* too few elements *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 0]; (* too many elements *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16];    (* no hole. *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 1; 11; 12; 13; 14; 15; 0];      (* all numbers not represented. *)
  |]

let boards4_results =
  [|
    Some 0; Some 8; Some 16; Some 26;
    Some 34; Some 48; Some 50; Some 52;
    Some 80; None
  |]

let moves3 =
  [ 
    (0, Up, 1);      (* board#, move, board# *)
    (0, Down, -1);
    (0, Left, 2);
    (0, Right, -1);  (* board# = -1 means the move is invalid *)

    (1, Up, 3);
    (1, Down, 0);
    (1, Left, 4);
    (1, Right, -1);

    (2, Up, 5);
    (2, Down, -1);
    (2, Left, 6);
    (2, Right, 0);

    (3, Up, -1);
    (3, Down, 1);
    (3, Left, 7);
    (3, Right, -1);

    (4, Up, 8);
    (4, Down, 9);
    (4, Left, 10);
    (4, Right, 1);

    (7, Left, 11);
    (11, Left, -1)
  ]

let move_boards3 =
  [|
    [1; 2; 3; 4; 5; 6; 7; 8; 0];  (*  0 *)
    [1; 2; 3; 4; 5; 0; 7; 8; 6];  (*  1 *)
    [1; 2; 3; 4; 5; 6; 7; 0; 8];  (*  2 *)
    [1; 2; 0; 4; 5; 3; 7; 8; 6];  (*  3 *)
    [1; 2; 3; 4; 0; 5; 7; 8; 6];  (*  4 *)
    [1; 2; 3; 4; 0; 6; 7; 5; 8];  (*  5 *)
    [1; 2; 3; 4; 5; 6; 0; 7; 8];  (*  6 *)
    [1; 0; 2; 4; 5; 3; 7; 8; 6];  (*  7 *)
    [1; 0; 3; 4; 2; 5; 7; 8; 6];  (*  8 *)
    [1; 2; 3; 4; 8; 5; 7; 0; 6];  (*  9 *)
    [1; 2; 3; 0; 4; 5; 7; 8; 6];  (* 10 *)
    [0; 1; 2; 4; 5; 3; 7; 8; 6];  (* 11 *)
  |]

let moves4 =
  [ 
    (0, Up, 1);      (* board#, move, board# *)
    (0, Down, -1);
    (0, Left, 2);
    (0, Right, -1);  (* board# = -1 means the move is invalid *)

    (1, Up, 3);
    (1, Down, 0);
    (1, Left, 4);
    (1, Right, -1);

    (2, Up, 5);
    (2, Down, -1);
    (2, Left, 6);
    (2, Right, 0);

    (3, Up, 7);
    (3, Down, 1);
    (3, Left, 8);
    (3, Right, -1);

    (4, Up, 9);
    (4, Down, 10);
    (4, Left, 11);
    (4, Right, 1);

    (5, Up, 12);
    (5, Down, 2);
    (5, Left, 13);
    (5, Right, 14);

    (7, Up, -1);
    (11, Left, 15);
    (15, Left, -1);
  ]

let move_boards4 =
  [|
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 0];  (*  0 *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 0; 13; 14; 15; 12];  (*  1 *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 0; 15];  (*  2 *)
    [1; 2; 3; 4; 5; 6; 7; 0; 9; 10; 11; 8; 13; 14; 15; 12];  (*  3 *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 0; 11; 13; 14; 15; 12];  (*  4 *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 0; 12; 13; 14; 11; 15];  (*  5 *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 0; 14; 15];  (*  6 *)
    [1; 2; 3; 0; 5; 6; 7; 4; 9; 10; 11; 8; 13; 14; 15; 12];  (*  7 *)
    [1; 2; 3; 4; 5; 6; 0; 7; 9; 10; 11; 8; 13; 14; 15; 12];  (*  8 *)
    [1; 2; 3; 4; 5; 6; 0; 8; 9; 10; 7; 11; 13; 14; 15; 12];  (*  9 *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 15; 11; 13; 14; 0; 12];  (* 10 *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 10; 11; 13; 14; 15; 12];  (* 11 *)
    [1; 2; 3; 4; 5; 6; 0; 8; 9; 10; 7; 12; 13; 14; 11; 15];  (* 12 *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 10; 12; 13; 14; 11; 15];  (* 13 *)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 12; 0; 13; 14; 11; 15];  (* 14 *)
    [1; 2; 3; 4; 5; 6; 7; 8; 0; 9; 10; 11; 13; 14; 15; 12];  (* 11 *)
  |]


(*** Utility functions. ***)

let expect_equal_int grp i expected actual =
  if expected <> actual then
    begin
      printf "FAIL: test group %s, test %d\n%!" grp i;
      printf "-- expected: %d; actual: %d\n%!" expected actual;
      assert_bool grp false
    end
  else
    assert_bool grp true

let expect_equal_loc grp i expected actual =
  if expected <> actual then
    let (re, ce) = expected in
    let (ra, ca) = actual in
    begin
      printf "FAIL: test group %s, test %d\n%!" grp i;
      printf "-- expected: (%d, %d); actual: (%d, %d)\n%!" re ce ra ca;
      assert_bool grp false
    end
  else
    assert_bool grp true

let expect_failure label thunk =
  try
    thunk ();
    assert_bool label false
  with 
    | Failure _ -> ()  (* expect Failure ... *)
    | _ -> 
      (* but any other exception is an error *)
      begin
        printf "FAIL: expected a Failure exception; ";
        printf "but a different exception was raised\n%!";
        assert_bool label false  
      end

let unexpected_exception label exc =
  let msg =
    sprintf "%s: unexpected exception raised: %s"
      label
      (Printexc.to_string exc)
  in
    assert_bool msg false  

(*************************************************************************
 ****************************** The tests. *******************************
 **************************************************************************) 

(* ---------------------------------------------------------------------- 
 * Tests for boardrep.ml
 * ---------------------------------------------------------------------- *)

(* This line overrides the >:: ounit2 operator.
 * The default is to allow 60 seconds per test; this pushes it to
 * 600 seconds. *)
let (>::) s f = OUnitTest.TestLabel(s, OUnitTest.TestCase(OUnitTest.Long, f))

let tests_arrayrep_init = "init tests for array rep" >:::
[
  "init tests (array rep)" >:: (fun _ ->
    (* Test error checking. *)
    expect_failure "init array fail 1" (fun () -> ArrayRep.init 0);
    expect_failure "init array fail 2" (fun () -> ArrayRep.init (-1));
    expect_failure "init array fail 3" (fun () -> ArrayRep.init 1);

    try
      let b3  = ArrayRep.init 3 in
      let b3c = CA.get_contents b3 in
      let b3v = List.map snd b3c in
        begin
          expect_equal_int "init array 3, get_size" 1 (ArrayRep.get_size b3) 3;
          expect_equal_loc "init array 3, get_hole" 1 (ArrayRep.get_hole b3) (2, 2);

          assert_bool "init array 3, 1"
            (CA.is_equal b3c solved3_all);
          assert_bool "init array 3, 2"
            (CA.is_equal b3v solved3);
        end
    with e ->
      unexpected_exception "init array 3, (1 or 2)" e;

    try
      let b4c = CA.get_contents (ArrayRep.init 4) in
      let b4v = List.map snd b4c in
        begin
          assert_bool "init array 4, 1"
            (CA.is_equal b4c solved4_all);
          assert_bool "init array 4, 2"
            (CA.is_equal b4v solved4);
        end
    with e ->
      unexpected_exception "init array 4, (1 or 2)" e;

    try
      let b5c = CA.get_contents (ArrayRep.init 5) in
      let b5v = List.map snd b5c in
        begin
          assert_bool "init array 5, 1"
            (CA.is_equal b5c solved5_all);
          assert_bool "init array 5, 2"
            (CA.is_equal b5v solved5);
        end
    with e ->
      unexpected_exception "init array 5, (1 or 2)" e
  );
]

let tests_arrayrep_load = "load tests for array rep" >:::
[
  "load tests (array rep)" >:: (fun _ ->
    (* Test error checking. *)
    expect_failure "load array fail 1" (fun () -> ArrayRep.load 0 []);
    expect_failure "load array fail 2" (fun () -> ArrayRep.load (-1) []);
    expect_failure "load array fail 3" (fun () -> ArrayRep.load 1 [0]);
    expect_failure "load array fail 4" (fun () -> ArrayRep.load 2 []);
    expect_failure "load array fail 5" (fun () -> ArrayRep.load 2 [1; 2; 3; 4; 0]);
    expect_failure "load array fail 6" (fun () -> ArrayRep.load 2 [0; 1; 2; 0]);
    expect_failure "load array fail 7" (fun () -> ArrayRep.load 2 [1; 2; 3; 4]);
    expect_failure "load array fail 8" (fun () -> ArrayRep.load 2 [1; 2; 3; 1]);

    List.iter
      (fun i -> 
         try
           let bl = boards3.(i) in
           let b  = ArrayRep.load 3 bl in
           let bc = CA.get_contents b in
           let bv = CA.get_values b in
           let bc_correct = List.combine boards3_locs bl in
             begin
               assert_bool "load array 3, 1"
                 (CA.is_equal bc bc_correct);
               assert_bool "load array 3, 2"
                 (CA.is_equal bv bl)
             end
         with e ->
           unexpected_exception "load array 3, (1 or 2)" e
      )
      [0; 1; 2; 3; 4; 5; 6; 7; 8];

    (* Check that invalid lists give rise to Failure exceptions. *)
    List.iter
      (fun i -> 
         try
           let thunk () =
             let bl = bad_boards3.(i) in
               ignore (ArrayRep.load 3 bl)
           in
             expect_failure "load array 3, 3" thunk
         with e ->
           unexpected_exception "load array 3, 3" e
      )
      [0; 1; 2; 3];

    List.iter
      (fun i -> 
         try
           let bl = boards4.(i) in
           let b  = ArrayRep.load 4 bl in
           let bc = CA.get_contents b in
           let bv = CA.get_values b in
           let bc_correct = List.combine boards4_locs bl in
             begin
               assert_bool "load array 4, 1"
                 (CA.is_equal bc bc_correct);
               assert_bool "load array 4, 2"
                 (CA.is_equal bv bl)
             end
         with e ->
           unexpected_exception "load array 4, (1 or 2)" e
      )
      [0; 1; 2; 3; 4; 5; 6; 7; 8; 9];

    (* Check that invalid lists give rise to Failure exceptions. *)
    List.iter
      (fun i -> 
         try
           let thunk () =
             let bl = bad_boards4.(i) in
               ignore (ArrayRep.load 4 bl)
           in
             expect_failure "load array 4, 3" thunk
         with e ->
           unexpected_exception "load array 4, 3" e
      )
      [0; 1; 2; 3];
  );
]

let tests_arrayrep_make_move = "make_move tests for array rep" >:::
[
  "move tests (array rep)" >:: (fun _ ->
    List.iteri
      (fun i (board_from, move, board_to) ->
         try
           let rep = "array" in
           let size = 3 in
           let msg_a = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tarray was not copied\n")
           in
           let msg_b = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tcorrect board was not generated\n")
           in
           let msg_c = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tillegal move didn't raise Invalid_move exception\n")
           in
           let msg_c2 = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tillegal move raised Invalid_location instead of Invalid_move exception\n")
           in
           let msg_d = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tlegal move raised Invalid_move exception\n")
           in
           let msg_d2 = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tlegal move raised Invalid_location exception\n")
           in
           let b1  = ArrayRep.load 3 move_boards3.(board_from) in
           let b1c = CA.get_contents b1 in
           let b1s = ArrayRep.get_size b1 in
           let b1h = ArrayRep.get_hole b1 in
             if board_to <> -1 then
               try
                 (* We expect that a particular move from board b1
                    will produce board b2. *)
                 let b2 = ArrayRep.load 3 move_boards3.(board_to) in
                 (* It actually produces board b3. *)
                 let b3 = ArrayRep.make_move b1 move in
                 (* Check that b1 hasn't been altered. *)
                 let b1c' = CA.get_contents b1 in
                 let b1s' = ArrayRep.get_size b1 in
                 let b1h' = ArrayRep.get_hole b1 in
                 (* Check that b2 and b3 are the same. *)
                 let b2c = CA.get_contents b2 in
                 let b3c = CA.get_contents b3 in
                 let b2s = ArrayRep.get_size b2 in
                 let b3s = ArrayRep.get_size b3 in
                 let b2h = ArrayRep.get_hole b2 in
                 let b3h = ArrayRep.get_hole b3 in
                   begin
                     (* Report failure to copy the array: *)
                     assert_bool msg_a (CA.is_equal b1c b1c');
                     assert_bool msg_a (b1s = b1s');
                     assert_bool msg_a (b1h = b1h');
                     (* Report failure to generate the correct board: *)
                     assert_bool msg_b (CA.is_equal b2c b3c);
                     assert_bool msg_b (b2s = b3s);
                     assert_bool msg_b (b2h = b3h);
                   end
               with 
                 | ArrayRep.Invalid_move ->
                   (* A legal move raised an Invalid_move exception. *)
                   assert_bool msg_d false
                 | ArrayRep.Invalid_location ->
                   (* A legal move raised an Invalid_location exception. *)
                   assert_bool msg_d2 false
             else
               (* We expect that an Invalid_move exception is raised. *)
               try
                 ignore (ArrayRep.make_move b1 move);
                 (* Report failure to raise the correct exception: *)
                 assert_bool msg_c false
               with 
                 | ArrayRep.Invalid_move -> ()
                 (* Wrong exception was raised: *)
                 | ArrayRep.Invalid_location -> assert_bool msg_c2 false
         with e ->
           unexpected_exception "move array 3" e
      )
      moves3;

    List.iteri
      (fun i (board_from, move, board_to) ->
         try
           let rep = "array" in
           let size = 4 in
           let msg_a = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tarray was not copied\n")
           in
           let msg_b = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tcorrect board was not generated\n")
           in
           let msg_c = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tillegal move didn't raise Invalid_move exception\n")
           in
           let msg_c2 = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tillegal move raised Invalid_location instead of Invalid_move exception\n")
           in
           let msg_d = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tlegal move raised Invalid_move exception\n")
           in
           let msg_d2 = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tlegal move raised Invalid_location exception\n")
           in
           let b1  = ArrayRep.load 4 move_boards4.(board_from) in
           let b1c = CA.get_contents b1 in
           let b1s = ArrayRep.get_size b1 in
           let b1h = ArrayRep.get_hole b1 in
             if board_to <> -1 then
               try
                 (* We expect that a particular move from board b1
                    will produce board b2. *)
                 let b2 = ArrayRep.load 4 move_boards4.(board_to) in
                 (* It actually produces board b3. *)
                 let b3 = ArrayRep.make_move b1 move in
                 (* Check that b1 hasn't been altered. *)
                 let b1c' = CA.get_contents b1 in
                 let b1s' = ArrayRep.get_size b1 in
                 let b1h' = ArrayRep.get_hole b1 in
                 (* Check that b2 and b3 are the same. *)
                 let b2c = CA.get_contents b2 in
                 let b3c = CA.get_contents b3 in
                 let b2s = ArrayRep.get_size b2 in
                 let b3s = ArrayRep.get_size b3 in
                 let b2h = ArrayRep.get_hole b2 in
                 let b3h = ArrayRep.get_hole b3 in
                   begin
                     (* Report failure to copy the array: *)
                     assert_bool msg_a (CA.is_equal b1c b1c');
                     assert_bool msg_a (b1s = b1s');
                     assert_bool msg_a (b1h = b1h');
                     (* Report failure to generate the correct board: *)
                     assert_bool msg_b (CA.is_equal b2c b3c);
                     assert_bool msg_b (b2s = b3s);
                     assert_bool msg_b (b2h = b3h);
                   end
               with
                 | ArrayRep.Invalid_move ->
                   (* A legal move raised an Invalid_move exception. *)
                   assert_bool msg_d false
                 | ArrayRep.Invalid_location ->
                   (* A legal move raised an Invalid_location exception. *)
                   assert_bool msg_d2 false
             else
               (* We expect that an Invalid_move exception is raised. *)
               try
                 ignore (ArrayRep.make_move b1 move);
                 (* Report failure to raise the correct exception: *)
                 assert_bool msg_c false
               with 
                 | ArrayRep.Invalid_move -> ()
                 (* Wrong exception was raised: *)
                 | ArrayRep.Invalid_location -> assert_bool msg_c2 false
         with e ->
           unexpected_exception "move array 4" e
      )
      moves4;
  );
]

let tests_maprep_init = "init tests for map rep" >:::
[
  "init tests (map rep)" >:: (fun _ ->
    (* Test error checking. *)
    expect_failure "init map fail 1" (fun () -> MapRep.init 0);
    expect_failure "init map fail 2" (fun () -> MapRep.init (-1));
    expect_failure "init map fail 3" (fun () -> MapRep.init 1);

    try
      let b3  = MapRep.init 3 in
      let b3c = CM.get_contents b3 in
      let b3v = List.map snd b3c in
        begin
          expect_equal_int "init map 3, get_size" 1 (MapRep.get_size b3) 3;
          expect_equal_loc "init map 3, get_hole" 1 (MapRep.get_hole b3) (2, 2);

          assert_bool "init map 3, 1"
            (CM.is_equal b3c solved3_all);
          assert_bool "init map 3, 2"
            (CM.is_equal b3v solved3);
        end
    with e ->
      unexpected_exception "init map 3, (1 or 2)" e;

    try
      let b4c = CM.get_contents (MapRep.init 4) in
      let b4v = List.map snd b4c in
        begin
          assert_bool "init map 4, 1"
            (CM.is_equal b4c solved4_all);
          assert_bool "init map 4, 2"
            (CM.is_equal b4v solved4);
        end
    with e ->
      unexpected_exception "init map 4, (1 or 2)" e;

    try
      let b5c = CM.get_contents (MapRep.init 5) in
      let b5v = List.map snd b5c in
        begin
          assert_bool "init map 5, 1"
            (CM.is_equal b5c solved5_all);
          assert_bool "init map 5, 2"
            (CM.is_equal b5v solved5);
        end
    with e ->
      unexpected_exception "init map 5, (1 or 2)" e
  );
]

let tests_maprep_load = "load tests for map rep" >:::
[
  "load tests (map rep)" >:: (fun _ ->
    (* Test error checking. *)
    expect_failure "load map fail 1" (fun () -> MapRep.load 0 []);
    expect_failure "load map fail 2" (fun () -> MapRep.load (-1) []);
    expect_failure "load map fail 3" (fun () -> MapRep.load 1 [0]);
    expect_failure "load map fail 4" (fun () -> MapRep.load 2 []);
    expect_failure "load map fail 5" (fun () -> MapRep.load 2 [1; 2; 3; 4; 0]);
    expect_failure "load map fail 6" (fun () -> MapRep.load 2 [0; 1; 2; 0]);
    expect_failure "load map fail 7" (fun () -> MapRep.load 2 [1; 2; 3; 4]);
    expect_failure "load map fail 8" (fun () -> MapRep.load 2 [1; 2; 3; 1]);
    expect_failure "load map fail 9" (fun () -> MapRep.load 2 [1; 2; 4; 0]);

    List.iter
      (fun i -> 
         try
           let bl = boards3.(i) in
           let b  = MapRep.load 3 bl in
           let bc = CM.get_contents b in
           let bv = CM.get_values b in
           let bc_correct = List.combine boards3_locs bl in
             begin
               assert_bool "load map 3, 1"
                 (CM.is_equal bc bc_correct);
               assert_bool "load map 3, 2"
                 (CM.is_equal bv bl)
             end
         with e ->
           unexpected_exception "load map 3, (1 or 2)" e
      )
      [0; 1; 2; 3; 4; 5; 6; 7; 8];

    (* Check that invalid lists give rise to Failure exceptions. *)
    List.iter
      (fun i -> 
         let thunk () =
           let bl = bad_boards3.(i) in
             ignore (MapRep.load 3 bl)
         in
           expect_failure "load map 3, 3" thunk)
      [0; 1; 2; 3];

    List.iter
      (fun i -> 
         try
           let bl = boards4.(i) in
           let b  = MapRep.load 4 bl in
           let bc = CM.get_contents b in
           let bv = CM.get_values b in
           let bc_correct = List.combine boards4_locs bl in
             begin
               assert_bool "load map 4, 1"
                 (CM.is_equal bc bc_correct);
               assert_bool "load map 4, 2"
                 (CM.is_equal bv bl)
             end
         with e ->
           unexpected_exception "load map 4, (1 or 2)" e
      )
      [0; 1; 2; 3; 4; 5; 6; 7; 8; 9];

    (* Check that invalid lists give rise to Failure exceptions. *)
    List.iter
      (fun i -> 
         try
           let thunk () =
             let bl = bad_boards4.(i) in
               ignore (MapRep.load 4 bl)
           in
             expect_failure "load map 4, 3" thunk
         with e ->
           unexpected_exception "load map 4, 3" e
      )
      [0; 1; 2; 3];
  );
]

let tests_maprep_make_move = "make_move tests for map rep" >:::
[
  "make_move tests (map rep)" >:: (fun _ ->
    List.iteri
      (fun i (board_from, move, board_to) ->
         try
           let rep = "map" in
           let size = 3 in
           let msg_a = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tcorrect board was not generated\n")
           in
           let msg_b = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tillegal move didn't raise Invalid_move exception\n")
           in
           let msg_b2 = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tillegal move raised Invalid_location instead of Invalid_move exception\n")
           in
           let msg_c = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tlegal move raised Invalid_move exception\n")
           in
           let msg_c2 = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tlegal move raised Invalid_location exception\n")
           in
           let b1  = MapRep.load 3 move_boards3.(board_from) in
             if board_to <> -1 then
               try
                 (* We expect that a particular move from board b1
                    will produce board b2. *)
                 let b2 = MapRep.load 3 move_boards3.(board_to) in
                 (* It actually produces board b3. *)
                 let b3 = MapRep.make_move b1 move in
                 (* Check that b2 and b3 are the same. *)
                 let b2c = CM.get_contents b2 in
                 let b3c = CM.get_contents b3 in
                 let b2s = MapRep.get_size b2 in
                 let b3s = MapRep.get_size b3 in
                 let b2h = MapRep.get_hole b2 in
                 let b3h = MapRep.get_hole b3 in
                   begin
                     (* Report failure to generate the correct board: *)
                     assert_bool msg_a (CM.is_equal b2c b3c);
                     assert_bool msg_a (b2s = b3s);
                     assert_bool msg_a (b2h = b3h);
                   end
               with
                 | MapRep.Invalid_move ->
                   (* A legal move raised an Invalid_move exception. *)
                   assert_bool msg_c false
                 | MapRep.Invalid_location ->
                   (* A legal move raised an Invalid_location exception. *)
                   assert_bool msg_c2 false
             else
               (* We expect that an Invalid_move exception is raised. *)
               try
                 ignore (MapRep.make_move b1 move);
                 (* Report failure to raise the correct exception: *)
                 assert_bool msg_b false
               with 
                 | MapRep.Invalid_move -> ()
                 (* Wrong exception was raised: *)
                 | MapRep.Invalid_location -> assert_bool msg_b2 false
         with e ->
           unexpected_exception "move map 3" e
      )
      moves3;

    List.iteri
      (fun i (board_from, move, board_to) ->
         try
           let rep = "map" in
           let size = 4 in
           let msg_a = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tcorrect board was not generated\n")
           in
           let msg_b = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tillegal move didn't raise Invalid_move exception\n")
           in
           let msg_b2 = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tillegal move raised Invalid_location instead of Invalid_move exception\n")
           in
           let msg_c = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tlegal move raised Invalid_move exception\n")
           in
           let msg_c2 = 
             (sprintf "make_move, %s rep, size %d, test %d:\n" rep size i)
             ^ (sprintf "\tlegal move raised Invalid_location exception\n")
           in
           let b1 = MapRep.load 4 move_boards4.(board_from) in
             if board_to <> -1 then
               try
                 (* We expect that a particular move from board b1
                    will produce board b2. *)
                 let b2 = MapRep.load 4 move_boards4.(board_to) in
                 (* It actually produces board b3. *)
                 let b3 = MapRep.make_move b1 move in
                 (* Check that b2 and b3 are the same. *)
                 let b2c = CM.get_contents b2 in
                 let b3c = CM.get_contents b3 in
                 let b2s = MapRep.get_size b2 in
                 let b3s = MapRep.get_size b3 in
                 let b2h = MapRep.get_hole b2 in
                 let b3h = MapRep.get_hole b3 in
                   begin
                     (* Report failure to generate the correct board: *)
                     assert_bool msg_a (CM.is_equal b2c b3c);
                     assert_bool msg_a (b2s = b3s);
                     assert_bool msg_a (b2h = b3h);
                   end
               with
                 | MapRep.Invalid_move ->
                   (* A legal move raised an Invalid_move exception. *)
                   assert_bool msg_c false
                 | MapRep.Invalid_location ->
                   (* A legal move raised an Invalid_location exception. *)
                   assert_bool msg_c2 false
             else
               (* We expect that an Invalid_move exception is raised. *)
               try
                 ignore (MapRep.make_move b1 move);
                 (* Report failure to raise the correct exception: *)
                 assert_bool msg_b false
               with 
                 | MapRep.Invalid_move -> ()
                 (* Wrong exception was raised: *)
                 | MapRep.Invalid_location -> assert_bool msg_b2 false
         with e ->
           unexpected_exception "move map 4" e
      )
      moves4;
  );

]

let _ = 
  begin
    Printf.printf "\nRUNNING PART A TESTS...\n";
    run_test_tt_main tests_arrayrep_init;
    run_test_tt_main tests_arrayrep_load;
    run_test_tt_main tests_arrayrep_make_move;
    run_test_tt_main tests_maprep_init;
    run_test_tt_main tests_maprep_load;
    run_test_tt_main tests_maprep_make_move;
  end


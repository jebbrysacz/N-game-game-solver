(** Entry point. *)

open Boardrep
open Boardmetric
open Npuzzle

module M1m = Hamming(MapRep)
module M2m = Manhattan(MapRep)

module M1a = Hamming(ArrayRep)
module M2a = Manhattan(ArrayRep)

module PHm = NPuzzle(MapRep)(M1m)
module PMm = NPuzzle(MapRep)(M2m)

module PHa = NPuzzle(ArrayRep)(M1a)
module PMa = NPuzzle(ArrayRep)(M2a)

(* 3x3 sample boards. *)
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

(* 4x4 sample boards. *)
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


let usage () = 
  print_endline
    ("usage: npuzzle [-s size] [-l num] [-r nrandom] [-a] [-h] [-i] [-help]\n"
     ^ "    -s size: side length of board (default 3)\n"
     ^ "      (e.g. 3 for 8-puzzle, 4 for 15-puzzle, etc.)\n"
     ^ "    -l num: load starting board at index `num` (>= 0)\n"
     ^ "    -r num: randomize the board by making `num` (>= 0) random moves\n"
     ^ "      starting from the solved state\n"
     ^ "    -a: use the array board representation instead of the map representation\n"
     ^ "    -h: use the Hamming metric instead of the Manhattan metric\n"
     ^ "    -i: don't solve the board; allow user to solve it interactively\n"
     ^ "    -help: print this help message\n"
     ^ "  default is equivalent to: -s 3 -l 0\n")

type init_opts =
  | Load of int  (* load board n for that size *)
  | Rand of int  (* make n random moves *)

type rep_opts =
  | Map
  | Array

type metric_opts =
  | H  (* Hamming *)
  | M  (* Manhattan *)

type args_info = {
  size : int;
  init : init_opts;
  rep  : rep_opts;
  met  : metric_opts;
  interactive : bool
}

let args_default = {
  size = 3;
  init = Load 0;
  rep  = Map;
  met  = M;
  interactive = false;
}

let rec get_args_info info args = 
  match args with
    | [] -> info
    | "-s" :: n :: rest ->
      let size = int_of_string n in
        if size < 1 then
          failwith "board size is too small"
        else
          get_args_info ({ info with size }) rest
    | "-l" :: n :: rest ->
      let index = int_of_string n in
        if index < 0 then
          failwith "board index is too small"
        else
          get_args_info ({ info with init = Load index }) rest
    | "-r" :: n :: rest ->
      let nmoves = int_of_string n in
        if nmoves < 0 then
          failwith "number of random moves is too small"
        else
          get_args_info ({ info with init = Rand nmoves }) rest
    | "-a" :: rest -> 
      get_args_info ({ info with rep = Array }) rest
    | "-h" :: rest -> 
      get_args_info ({ info with met = H }) rest
    | "-i" :: rest -> 
      get_args_info ({ info with interactive = true }) rest
    | "-help" :: _ -> raise Exit
    | arg :: _ -> 
      let msg = Printf.sprintf "invalid argument: %s" arg in
        failwith msg

module Run (S : Solver) =
  struct
    let run args_info = 
      let size = args_info.size in
      let init = args_info.init in
      let interactive = args_info.interactive in
      let start = 
        match init with
          | Rand n -> S.init size n
          | Load i ->
            if size = 3 then
              (if i >= Array.length boards3 then
                 failwith "sample board index out of range"
               else
                 S.load 3 boards3.(i))
            else if size = 4 then
              (if i >= Array.length boards4 then
                 failwith "sample board index out of range"
               else
                 S.load 4 boards4.(i))
            else
              failwith "sorry, no sample boards at that size"
      in
        if interactive then
          S.interact start
        else
          match S.solve true start with
            | Some solution -> 
              let len = List.length solution - 1 in
              let move = if len = 1 then "move" else "moves" in
              begin
                S.print solution;
                Printf.printf "Solution found in %d %s.\n%!" len move;
                if not (S.check_solution solution) then
                  begin
                    Printf.printf "\nERROR: solution found is not valid! ";
                    Printf.printf "  Check output in ./npuzzle.out\n%!"
                  end
              end
            | None -> Printf.printf "No solution found.\n%!"
  end

let _ = 
  let _ = Random.self_init () in
  let args = List.tl (Array.to_list Sys.argv) in
    try
      let args_info = get_args_info args_default args in
        match args_info with
          | { rep = Map;   met = H; _ } -> 
            let module S = Run(PHm) in S.run args_info
          | { rep = Map;   met = M; _ } ->
            let module S = Run(PMm) in S.run args_info
          | { rep = Array; met = H; _ } ->
            let module S = Run(PHa) in S.run args_info
          | { rep = Array; met = M; _ } ->
            let module S = Run(PMa) in S.run args_info
    with 
      | (Failure msg) ->  
        begin
          Printf.printf "ERROR: %s\n%!" msg;
          usage ()
        end
      | Exit -> usage ()


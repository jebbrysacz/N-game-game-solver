(** Solving the N-puzzle game. *)

open Boardrep
open Board
open Boardmetric

(** Module type that describes N-puzzle solvers. *)
module type Solver =
  sig
    module B : Board

    (** Make the initial board.
        The first argument is the side length of the board
        (e.g. 4 for the 15-puzzle).
        The second is the number of random moves used to
        scramble the solved board. *)
    val init : int -> int -> B.t

    (** Load the initial board from a list.
        The first argument is the side length of the board
        (e.g. 4 for the 15-puzzle).
        The second is a list of integers representing the contents of
        the board as a concatenation of rows. *)
    val load : int -> int list -> B.t

    (** Solve a board.
        If successful, return the list of boards starting from
        the starting board and ending in the solved configuration.
        If unsuccessful, return None. 
        The first argument is true if you want the solver to print out
        the initial board configuration as a list of integers. *)
    val solve : bool -> B.t -> B.t list option

    (** Print a list of boards (normally the solution) to a file 
        called "npuzzle.out". *)
    val print : B.t list -> unit

    (** Allow the user to solve the board interactively.  For testing. *)
    val interact : B.t -> unit

    (** Check a solution for validity. *)
    val check_solution : B.t list -> bool
  end
  

(** Create the N-puzzle solver. *)
module NPuzzle (BR : BoardRep) (M : BoardMetric with type t = BR.t) : Solver


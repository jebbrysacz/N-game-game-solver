(** The N-puzzle game board. *)

open Boardrep
open Boardmetric

(** The interface of the game board. *)
module type Board =
  sig

    (** The type of the board. *)
    type t

    (** Given an integer N, create an NxN board filled with numbers
        in the solution configuration. *)
    val init : int -> t

    (** Load the initial board from a list.
        The first argument is the side length of the board
        (e.g. 4 for the 15-puzzle).
        The second is a list of integers representing the contents of
        the board as a concatenation of rows. *)
    val load : int -> int list -> t

    (** Get the size of the board. *)
    val get_size : t -> int

    (** Get the contents of a particular location. 
        Raises `Not_found` if the location is not on the board. *)
    val get : t -> loc -> int

    (** Randomize a board by making N random moves from a configuration. *)
    val randomize : t -> int -> t

    (** Compare two boards for ordering. *)
    val compare : t -> t -> int

    (** Evaluate the board's goodness of fit relative to the solution
        configuration.  The first argument is the solution configuration. *)
    val eval : t -> t -> int

    (** Given a board, return a list of all boards that can be obtained
        by moving the hole in any direction. *)
    val next : t -> t list

    (** Return a list of all the indices and elements of the board, 
        as a list of (coordinate, value) pairs.  For testing. *)
    val get_contents : t -> (loc * int) list

    (** Return a string representing the board, for printing purposes. *)
    val display : t -> string

    (** Return a string representing the board as a single line.
        For testing. *)
    val dump : t -> string

    (** Interactively try to solve a board.  For testing. *)
    val interact : t -> unit

    (** Check that a board is in a valid state.
        Print an error message and return false if not. *)
    val check_board : t -> bool

  end


module Make (B : BoardRep) (M : BoardMetric with type t = B.t) : Board

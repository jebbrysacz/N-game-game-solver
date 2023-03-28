(** The N-puzzle board representation. *)

type loc = int * int
type move = Up | Down | Left | Right

module type BoardRep =
  sig
    (** The abstract type of the board representation. *)
    type t

    (** This exception is raised when a move would put the hole off the
        edge of the board. *)
    exception Invalid_move

    (** This exception is raised when `get` is asked to retrieve a value
        from an invalid location. *)
    exception Invalid_location

    (** Make an NxN board in winning position. 
        The first argument is the size of the board (N).
        The contents are the integers between 1 and NxN
        inclusive.  The hole is represented by the number 0. *)
    val init : int -> t

    (** Create an NxN board from a list of numbers.
        The list should contain only the integers from 0 to N*N-1,
        with no repeats. *)
    val load : int -> int list -> t

    (** Get the size of the board. *)
    val get_size : t -> int

    (** Get the location of the hole on the board. *)
    val get_hole : t -> loc

    (** Get the contents of a particular location. 
        Raises `Not_found` if the location is not on the board. *)
    val get : t -> loc -> int

    (** Move the hole in the specified direction. 
        Raises `Invalid_move` if the direction is invalid i.e. if the
        move would put the hole off the board. *)
    val make_move : t -> move -> t

    (** Print the board state.  For debugging. *)
    val show : t -> unit
  end

module ArrayRep : BoardRep
module MapRep   : BoardRep


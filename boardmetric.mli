(** Metrics measuring the distance between a board configuration
    and the solved board configuration. *)

module type BoardMetric = 
  sig 
    (** The type of the board. *)
    type t 

    (** The "distance" between two boards. 
        By convention, the first argument is the solved board. *)
    val distance : t -> t -> int 
  end

(** The Hamming metric adds 1 for every location that has a wrong element. *)
module Hamming (B : Boardrep.BoardRep) : BoardMetric with type t = B.t

(** The Manhattan metric computes the sum of the distances (sum of the vertical 
    and horizontal distance) from the blocks to their goal positions. *)
module Manhattan (B : Boardrep.BoardRep) : BoardMetric with type t = B.t


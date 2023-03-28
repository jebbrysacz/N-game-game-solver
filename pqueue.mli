(** Priority queues. *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

(** Priority queue functor. 
    Given an ordered type, create a priority queue. *)
module MakePriorityQueue (Elt : OrderedType) :
  sig
    (** Raise this exception when trying to find/delete the minimum value
        in an empty queue. *)
    exception Empty

    (** The type of the queue elements. *)
    type elem = Elt.t

    (** The type of the queue itself. *)
    type t

    (** The empty queue. *)
    val empty : t

    (** Insert a value into the queue. *)
    val insert : t -> elem -> t

    (** Pop the minimum value off the priority queue.
        Return it and the queue without that element. *)
    val pop_min : t -> elem * t
  end


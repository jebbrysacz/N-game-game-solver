module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type PriorityQueue =
  sig
    exception Empty
    type elem
    type t

    val empty       : t
    val insert      : t -> elem -> t
    val pop_min     : t -> elem * t
  end

module MakePriorityQueue (Elt : OrderedType) 
  : (PriorityQueue with type elem = Elt.t) =
  struct
    module H = BatHeap.Make (Elt)

    exception Empty
    type elem = Elt.t
    type t = H.t

    let empty = H.empty
    let insert = H.insert
    let pop_min q = 
      try
        (H.find_min q, H.del_min q)
      with Invalid_argument _ ->
        raise Empty
  end


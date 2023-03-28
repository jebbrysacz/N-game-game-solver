open Boardrep

module type BoardMetric =
  sig
    type t

    val distance : t -> t -> int
  end

module Hamming(B : BoardRep) : BoardMetric with type t = B.t =
  struct
    type t = B.t

    let distance b1 b2 =
      if ((B.get_size b1) <> (B.get_size b2)) then
        failwith "boards must be same sizes to compare"
      else  
      let size = B.get_size b1 in
      let size2 = (size * size) in
      let rec iter b1 b2 acc n = 
        if n > size2 then
          acc
        else
        let loc = ((n-1) / size, (n-1) mod size) in
        let holder1 = B.get b1 loc in
        let holder2 = B.get b2 loc in
        if holder2 = 0 then
          iter b1 b2 acc (n+1)
        else if holder1 = holder2 then
          iter b1 b2 acc (n+1)
        else iter b1 b2 (acc+1) (n+1)
      in iter b1 b2 0 1

  end

module Manhattan(B : BoardRep) : BoardMetric with type t = B.t =
  struct
    type t = B.t

    let update arr loc idx = 
      arr.(idx-1) <- loc;
    arr

    let builder b1 = 
      let size = B.get_size b1 in
      let size2 = (size * size) in
      let rec iter b arr n =
        if n > size2 then
          arr 
        else 
          let loc = ((n-1) / size, (n-1) mod size) in
          let elem = B.get b loc in
          match elem with
          | 0 -> iter b arr (n+1)
          | elem ->
            let arr1 = update arr loc elem in
            iter b arr1 (n+1)
      in iter b1 (Array.make (size2 - 1) (0,0)) 1

    (*let rec builder binding arr =
      match binding with
      | [] -> arr
      | h :: t when (snd h = 0)->
          builder t arr
      | h :: t ->
        let i = snd h in
        let loc = fst h in
        let new_arr = update arr loc i in
          builder t new_arr*) 

    let comp_loc loc1 loc2 = 
      let x1 = fst loc1 in
      let x2 = fst loc2 in
      let y1 = snd loc1 in
      let y2 = snd loc2 in
      let diff1 = x1 - x2 in
      let diff2 = y1 - y2 in
      let abs1 = abs diff1 in
      let abs2 = abs diff2 in
      (abs1 + abs2)

    let distance b1 b2 =
      if ((B.get_size b1) <> (B.get_size b2)) then
        failwith "boards must be same sizes to compare"
      else
        let length = (B.get_size b1) * (B.get_size b1) - 1 in
        let bindings1 = builder b1 in
        let bindings2 = builder b2 in
        let rec comp_b arr1 arr2 acc i = 
          match i with 
          | i when i=length -> acc
          | i -> 
            let loc1 = Array.get arr1 i in
            let loc2 = Array.get arr2 i in
            let value = comp_loc loc1 loc2 in
            comp_b arr1 arr2 (acc + value) (i+1)
        in comp_b bindings1 bindings2 0 0
        (*let ar1 = Array.make ((size * size - 1)) (0,0) in
        let ar2 = Array.make ((size * size - 1)) (0,0) in
        let arr1 = bindings1 ar1 in
        let arr2 = bindings2 ar2 in
        let rec comp_b arr1 arr2 acc i =
          match i with
          | i when i = (Array.length ar1) ->
            acc
          | i -> 
            let loc1 = Array.get arr1 i in
            let loc2 = Array.get arr2 i in
            let value = comp_loc loc1 loc2 in
            comp_b arr1 arr2 (acc + value) (i+1)
        in arr1 arr2 0 0*)

          
  end


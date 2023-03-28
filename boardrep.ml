type loc = int * int
type move = Up | Down | Left | Right

module type BoardRep =
  sig
    type t

    exception Invalid_move
    exception Invalid_location

    val init      : int -> t
    val load      : int -> int list -> t
    val get_size  : t -> int
    val get_hole  : t -> loc
    val get       : t -> loc -> int
    val make_move : t -> move -> t
    val show      : t -> unit
  end

(* ---------------------------------------------------------------------- 
 * Helper functions.
 * ---------------------------------------------------------------------- *)

(* Make a display function given board accessors. *)
let make_show get get_size b =
  let size = get_size b in
    begin
      Printf.printf "\n%!";
      for row = 0 to size - 1 do
        for col = 0 to size - 1 do
          Printf.printf "%3d" (get b (row, col))
        done;
        Printf.printf "\n";
      done;
      Printf.printf "\n%!"
    end

(* ---------------------------------------------------------------------- 
 * Modules.
 * ---------------------------------------------------------------------- *)

module OrderedLoc =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module ArrayRep : BoardRep =
  struct
    type t = 
      { 
        acontents : int array;
        size : int;
        hole : loc
      }

    exception Invalid_move
    exception Invalid_location

    let init size = 
      if size < 2 then
        failwith "ERROR: init: size must be at least 2"
      else
        let f = (fun x -> x+1) in
        let num = (size * size) - 1 in
        let fst = Array.init num f in
        let snd = Array.make 1 0 in
        let arr = Array.append fst snd in
        {acontents = arr; 
         size = size;
         hole = (size - 1, size - 1)}

    let load size lst =
      if size < 2 then
        failwith "ERROR: load: size must be at least 2"
      else
      if (List.length lst) != (size * size) then
        failwith "ERROR: load: list length must be size * size"
      else
      let checker lst = 
        let rec help lst n = 
          match n with
          | -1 -> true
          | _ -> 
            match (List.mem n lst) with
            | true -> help lst (n - 1)
            | false -> false
        in help lst ((size * size) - 1)
      in
      if not (checker lst) then
        failwith "ERROR: load: list must contain elements 0 to size^2 - 1"
      else
        let find_hole lst =
          let rec iter lst n =
            match lst with
            | [] -> failwith "this should not happen"
            | h :: _ when (h = 0) ->
              n
            | _ :: t -> iter t (n + 1)
          in iter lst 0
        in
        let ind = find_hole lst in
        let col = ind mod size in
        let row = ind / size in
        let rec help lst arr = 
          match lst with 
          | [] -> 
            {acontents = arr;
             size = size;
             hole = (row, col)}
          | h :: t -> 
            let to_add = Array.make 1 h in
            help t (Array.append arr to_add)
        in help lst (Array.make 0 0)

    let get_size b = b.size

    let get_hole b = b.hole

    let get { acontents; size = s; _ } (r, c) = 
      if r < 0 || r > s - 1 || c < 0 || c > s - 1 then
        raise Invalid_location
      else
        try
          acontents.(r * s + c)
        with (Invalid_argument _) ->
          raise Invalid_location

    let swap arr id1 num1 id2 num2 =
      arr.(id1) <- num2;
      arr.(id2) <- num1;
    arr

    let make_move b m = 
      try 
      let move mov (row, col) =
        match mov with
        | Up -> (row - 1, col)
        | Down -> (row + 1, col)
        | Right -> (row, col + 1)
        | Left -> (row, col - 1)
      in
      let old_hole = get_hole b in
      let new_hole = move m old_hole in 
      let num = get b new_hole in
      let old_rep = b.acontents in
      let old_idx = ((fst old_hole) * (get_size b)) + (snd old_hole) in
      let new_idx = ((fst new_hole) * (get_size b)) + (snd new_hole) in
      let thing = swap (Array.copy old_rep) new_idx num old_idx 0 in
      {acontents = thing; size = get_size b; hole = new_hole}
      with 
      Invalid_location -> raise Invalid_move

    let show = make_show get get_size
  end

module MapRep : BoardRep =
  struct
    module LocMap = Map.Make(OrderedLoc)

    type t = 
      { 
        mcontents : int LocMap.t;
        size : int;
        hole : loc
      }

    exception Invalid_move
    exception Invalid_location

    let init size =
      if size < 2 then
        failwith "ERROR: init: size must be at least 2"
      else
        let rec iter n map =
          match n with
          | n when n >= (size * size) ->
            map
          | n -> 
            let loc = ((n-1) / size, (n-1) mod size) in
            iter (n+1) (LocMap.add loc n map)
        in
        let map = LocMap.empty in
        let map1 = iter 1 map in
        let hole_loc = (size - 1, size - 1) in
        let map2 = LocMap.add hole_loc 0 map1 in
        {mcontents = map2; 
         size = size;
         hole = hole_loc}

    let load size lst =
      if size < 2 then
        failwith "ERROR: load: size must be at least 2"
      else
        if (List.length lst) != (size * size) then
          failwith "ERROR: load: list length must be size * size"
        else
        let checker lst = 
          let rec help lst n = 
            match n with
            | -1 -> true
            | _ -> 
              match (List.mem n lst) with
              | true -> help lst (n - 1)
              | false -> false
          in help lst ((size * size) - 1)
        in
        if not (checker lst) then
          failwith "ERROR: load: list must contain elements 0 to size^2 - 1"
        else
          let find_hole lst =
            let rec iter lst n =
              match lst with
              | [] -> failwith "this should not happen"
              | h :: _ when (h = 0) ->
                n
              | _ :: t -> iter t (n + 1)
            in iter lst 0
          in
          let ind = find_hole lst in
          let col = ind mod size in
          let row = ind / size in
          let rec help lst map n = 
            match lst with 
            | [] -> 
              {mcontents = map;
               size = size;
               hole = (row, col)}
            | h :: t -> 
              let loc = ((n-1) / size, (n-1) mod size) in
              let mapping = LocMap.add loc h map in
              help t mapping (n+1)
          in help lst LocMap.empty 1

    let get_size b = b.size

    let get_hole b = b.hole

    let get { mcontents; _ } l = 
      try
        LocMap.find l mcontents
      with Not_found ->
        raise Invalid_location

    let swap map loc1 num1 loc2 num2 =
      let map1 = LocMap.add loc1 num2 map in
      let map2 = LocMap.add loc2 num1 map1 in
      map2

    let make_move b m = 
      try 
      let move mov (row, col) =
        match mov with
        | Up -> (row - 1, col)
        | Down -> (row + 1, col)
        | Right -> (row, col + 1)
        | Left -> (row, col - 1)
      in
      let old_hole = get_hole b in
      let new_hole = move m old_hole in 
      let num = get b new_hole in
      let old_rep = b.mcontents in
      let thing = swap old_rep new_hole num old_hole 0 in
      {mcontents = thing; size = get_size b; hole = new_hole}
      with 
      Invalid_location -> raise Invalid_move

    let show = make_show get get_size
  end


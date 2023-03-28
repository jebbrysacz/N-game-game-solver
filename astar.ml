open Pqueue

module type Task =
  sig
    type t

    val compare : t -> t -> int
    val eval    : t -> t -> int
    val next    : t -> t list
    val display : t -> string
  end

module AStar (T : Task) =
  struct
    (* Raised when no more task states are left on the queue. *)
    exception No_solution

    (* Raised when a solution is encountered while searching. *)
    exception Solved of T.t list

    (* The state of a search stored in the priority queue *)
    type qstate = {
      tstate : T.t;  (* a task state *)

      (* the list of task states that preceded this one in the search *)
      history : T.t list;  

      (* the number of moves to get to this state; this is just the length
         of the history list *)
      nmoves : int;   

      (* the overall fitness: a function of the tstate evaluation and nmoves *)
      fitness : int    
    }

    (* Make the priority queue.  Compare queue states by fitness. *)
    module Tqueue = MakePriorityQueue(struct
        type t = qstate
        let compare s1 s2 = Stdlib.compare s1.fitness s2.fitness
      end)

    (* A map of the best quality scores for each evaluated tstate. *)
    module Tmap = Map.Make(T)

    (* The state of the solver as a whole. *)
    type t = {
      queue : Tqueue.t;
      best  : int Tmap.t
    }

    (* The solver function. *)
    let solve goal init =
      let init_eval  = T.eval goal init in
      let init_state =
        {
           tstate  = init;
           history = [];
           nmoves  = 0;
           fitness = init_eval;
        }
      in
      let init_queue  = Tqueue.insert (Tqueue.empty) init_state in
      let init_best   = Tmap.empty in
      let init_solver = { queue = init_queue; best = init_best } in

      (* 
         The main solving loop using the priority queue.
       
         Invariant: tstates on the queue are not solved.

         1) Pop a qstate off the queue.  If the queue is empty,
            there is no solution.  Raise a No_solution exception.

         2) Check if the tstate in the popped qstate is a key in the `best`
            map: If it isn't, add it to the `best` map as a key with the number
            of moves leading to the task state (the `nmoves` field of the queue
            state) as the value.  If it is, compare it to the move count in the
            map.  If the new number of moves is smaller than the one in the map,
            replace the binding in the map.  Otherwise, this task has been
            already searched for with a smaller (or at least no larger) number
            of moves, so there is no point in searching it again, so discard it
            and restart the loop.

         3) Assuming we got this far, compute the next qstates.
            Check to see if any of them contains a tstate which is a solution;
            if so, compute the final list of tasks and raise a Solved exception
            to break out of the loop.
            Otherwise, add them back into the queue and continue.  
       *)

      let checker qstate best0 =
        let thing = Tmap.find_opt qstate.tstate best0 in
        match thing with
        | Some n ->
          let num = qstate.nmoves in
          if (num < n) then
            ((Tmap.add qstate.tstate num best0), num, false)
          else 
            (best0, n, true)
        | None ->
          let to_add = qstate.nmoves in
          ((Tmap.add qstate.tstate to_add best0), to_add, false)
      in

      let rec iter { queue; best }  =

        let (popped, queue1) = Tqueue.pop_min queue in
        let (best1, old_moves, check) = checker popped best in
        if check then
          iter {queue = queue1; best = best1}
        else
        let next_q = T.next popped.tstate in
        let rec iter1 nex_q queue0 qstate1 =
          match nex_q with
          | [] -> iter {queue = queue0; best = best1}
          | h :: _  when (T.compare goal h = 0) ->
            let lst = List.rev (h :: qstate1.tstate :: qstate1.history) in
            raise (Solved lst)
          | h :: t when (Tmap.mem h best1) = false ->
            let state1 = {tstate = h; 
                          history = qstate1.tstate :: qstate1.history;
                          nmoves = old_moves + 1;
                          fitness = T.eval goal h + old_moves} in
            let queue2 = Tqueue.insert queue0 state1 in
            iter1 t queue2 qstate1
          | _ :: t -> 
            iter1 t queue0 qstate1
        in iter1 next_q queue1 popped
      in (* The main part of the function starts here. *)

      if init_eval = 0 then
        [init]  (* handle the case when the initial state is solved. *)
      else
        try
          iter init_solver
        with 
          | Tqueue.Empty -> raise No_solution
          | Solved tlist -> tlist
  end


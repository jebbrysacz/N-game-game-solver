(** A* algorithm *)

(** A task that can be solved by an A* algorithm. *)
module type Task =
  sig
    (** The state of the task. *)
    type t

    (** Compare two task states for ordering. *)
    val compare : t -> t -> int

    (** Evaluate the task's goodness of fit relative to the solution
        configuration (which is the first argument). *)
    val eval : t -> t -> int

    (** Given a task, return a list of all tasks that can be obtained
        from it by making one "move". *)
    val next : t -> t list

    (** Convert a task to a string, for debugging purposes. *)
    val display : t -> string
  end

(** A* algorithm solver. *)
module AStar (T : Task) :
  sig
    (** Exception raised when the solver fails to find a solution. *)
    exception No_solution

    (** Solve the task, starting from an initial state.
        Arguments:
          -- the first task argument is the goal state of the task
          -- the second task argument is the initial state of the task
        Return: 
          a list of task states leading to the solved state. 
        Raise `No_solution` if no solution is found. 
      *)
    val solve : T.t -> T.t -> T.t list
  end



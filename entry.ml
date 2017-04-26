type todo = { id: int; text: string }

type state = { max_id : int; todos : todo list; }

type action = 
    | Create of string
    | Delete of string

let create state = function
    | Create(text) -> let max_id = state.max_id + 1 in
                      let todo = { text; id = max_id } in
                      { todos = todo::state.todos; max_id }
    | _ -> state;;

let delete state = function
    | Delete(text) -> { state with todos = List.filter (fun todo -> todo.text <> text) state.todos }
    | _ -> state;;

let reducers = [create; delete]

let reduce state action = 
                         List.fold_left (fun state reducer -> reducer state action)
                         state
                         reducers;;

let print_todos todos =
    List.iter (fun x -> print_string (" - " ^ x.text ^ "\n")) todos;
    print_newline ();;


module type StoreBase = sig
    type t
    val get_state : unit -> t
    val subscribe : (unit -> unit) -> unit
    val dispatch : action -> unit
    val dispatch_loudly : action -> (string -> unit) -> unit
end

module type Store = StoreBase with type t := state;;

module Store = struct
    type t = state
    let state_ref = ref { max_id = 1; todos = []; }
    let subscribers = ref []
    let get_state () : t = !state_ref
    let changed () = List.iter (fun subscriber -> subscriber ()) !subscribers
    let subscribe fn = subscribers := fn::!subscribers
    let dispatch action = state_ref := reduce !state_ref action; changed ();;

end

let string_of_action = function
        | Create(text) -> "Create(" ^ text ^ ")"
        | Delete(text) -> "Delete(" ^ text ^ ")"

let dispatch action =
    action
        |> string_of_action
        |> print_string;
    print_newline ();
    Store.dispatch action;;


let () =
       Store.subscribe (fun () -> print_todos (Store.get_state ()).todos);
       dispatch (Create "brush teeth");
       dispatch (Create "buy groceries");
       dispatch (Delete "brush teeth");
       dispatch (Create "eat food groceries");;

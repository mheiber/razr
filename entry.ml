(* define a Redux-like library *)
module type Store = sig
    type state_t
    type action_t
    val get_state : unit -> state_t
    val unsubscribe : (unit -> unit) -> unit
    val subscribe : (unit -> unit) -> unit
    val dispatch : action_t -> unit
    val reduce : state_t -> action_t -> state_t
end

module type Store_def = sig
    type state_t
    type action_t
    val state : state_t
    val reduce : state_t -> action_t -> state_t
end

module MakeStore(Def: Store_def) : (Store
    with type action_t := Def.action_t
    with type state_t := Def.state_t) = struct
    let state_ref = ref Def.state
    let subscribers = ref []
    let get_state () = !state_ref
    let reduce x = Def.reduce x
    let unsubscribe subscriber = subscribers := List.filter (fun s -> s != subscriber) !subscribers
    let subscribe subscriber = subscribers := subscriber::!subscribers
    let _changed () = List.iter (fun subscriber -> subscriber ()) !subscribers
    let dispatch action = state_ref := reduce !state_ref action; _changed ();;
end

(* use the library to create a store *)

type todo = { id: int; text: string }
type state = { max_id : int; todos : todo list; }
type action =
    | Create of string
    | Delete of string

module Store = MakeStore(struct
    type state_t = state
    type action_t = action
    let state = { max_id = 1; todos = []; } 
    let reduce state = function
        | Create(text) -> let max_id = state.max_id + 1 in
                        let todo = { text; id = max_id } in
                        { todos = todo::state.todos; max_id }
        | Delete(text) -> { state with todos = List.filter (fun todo -> todo.text <> text) state.todos }
end)

(* utils for logging *)

let print_todos todos =
    List.iter (fun x -> print_string (" - " ^ x.text ^ "\n")) todos;
    print_newline ();;

let string_of_action = function
        | Create(text) -> "Create(" ^ text ^ ")"
        | Delete(text) -> "Delete(" ^ text ^ ")"

let dispatch action =
    action
        |> string_of_action
        |> print_string;
    print_newline ();
    Store.dispatch action;;

let print_state state = print_todos ((Store.get_state ()).todos)

(* do stuff *)

let () =
       Store.subscribe print_state;
       dispatch (Create "brush teeth");
       dispatch (Create "buy groceries");
       dispatch (Delete "brush teeth");
       Store.unsubscribe print_state;;
       dispatch (Create "eat food groceries");

(* Output:

    Create(brush teeth)
    - brush teeth

    Create(buy groceries)
    - buy groceries
    - brush teeth

    Delete(brush teeth)
    - buy groceries

    Create(eat food groceries)

*)
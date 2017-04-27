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
(*val get_state : unit -> t*)

module type StoreBase = sig
    type t
    val subscribe : (unit -> unit) -> unit
    val dispatch : action -> unit
end

module type Z = sig
    val x : int
end
module Z = struct
    let x = 2
end


let create_Store state = 
    (module struct
        type t
        let state_ref = ref state
        let subscribers = ref []
        let get_state () = !state_ref
        let changed () = List.iter (fun subscriber -> subscriber ()) !subscribers
        let subscribe subscriber = subscribers := subscriber::!subscribers
        let dispatch action = state_ref := reduce !state_ref action; changed ();;
     end : StoreBase with type t := state) ;;

(*
module Store = struct
    type t = state
    let state_ref = ref { max_id = 1; todos = []; }
    let subscribers = ref []
    let get_state () : t = !state_ref
    let changed () = List.iter (fun subscriber -> subscriber ()) !subscribers
    let subscribe subscriber = subscribers := subscriber::!subscribers
    let dispatch action = state_ref := reduce !state_ref action; changed ();;

end*)

module Store = (val create_Store { max_id = 1; todos = []; } : StoreBase);;

let string_of_action = function
        | Create(text) -> "Create(" ^ text ^ ")"
        | Delete(text) -> "Delete(" ^ text ^ ")"

let dispatch action =
    action
        |> string_of_action
        |> print_string;
    print_newline ();
    Store.dispatch action;;

let print_state state = print_todos (Store.get_state ()).todos;;

let () =
       Store.subscribe print_state;
       dispatch (Create "brush teeth");
       dispatch (Create "buy groceries");
       dispatch (Delete "brush teeth");
       dispatch (Create "eat food groceries");;

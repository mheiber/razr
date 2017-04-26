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

let state = ref { max_id = 1; todos = []; }

let subscribers = ref []
let changed () = List.iter (fun subscriber -> subscriber ()) !subscribers;;
let dispatch action = state := reduce !state action; changed ();;
let subscribe fn = subscribers := fn::!subscribers;;

let () = subscribe (fun () -> print_string "new state:\n"; print_todos !state.todos);;

let string_of_action = function
        | Create(text) -> "Create(" ^ text ^ ")"
        | Delete(text) -> "Delete(" ^ text ^ ")"

let dispatch_loudly action = 
                                action
                                    |> string_of_action
                                    |> print_string;
                                print_newline ();
                                dispatch action;;


dispatch_loudly (Create "brush teeth");
dispatch_loudly (Create "eat food");
dispatch_loudly (Create "buy groceries");
dispatch_loudly (Delete "brush teeth");

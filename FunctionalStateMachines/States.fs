module states

type Event =
    | OnEnter
    | OnExit
    | Update
    | OnKeyDown of string
    | OnKeyUp of string
    | Custom of string

type ActionResult =
    | ActionFailure of string
    | ActionSuccess
    
type TransitionResult =
    | TransitionFailure of string
    | TransitionTo of int
    | NoTransition
    
type Action = Event -> ActionResult
type Transition = Event -> TransitionResult
    
type StatePtr =
    val mutable index:int
    new p = { index = p }
    member this.pointing_in states = Array.get states this.index
    
type State =
    | Flat of Action list * Transition list
    | Super of State array * int * StatePtr * Transition list


let private event_name = function
    | OnEnter -> "OnEnter"
    | OnExit -> "OnExit"
    | Update -> "Update"
    | OnKeyDown key -> $"OnKeyDown {key}"
    | OnKeyUp key -> $"OnKeyUp {key}"
    | Custom key -> $"Custom {key}"


let make_flat_state actions transitions = Flat (actions, transitions)
let make_super_state initial states transitions = Super (states, initial, StatePtr initial, transitions)
let make_root initial states = make_super_state initial states []

let trigger_event state event =        
    let rec rec_trigger state event = 
        
        let failure_flat event (pointer:StatePtr) message = ActionFailure $"{event_name event} {pointer.index} >> {message}"
        let error_deep message = TransitionFailure $"\n{message}"
        
        let failure_transition_on_exit pointer destination = failure_flat OnEnter pointer $"transition to {destination}"

        let trigger_flat states (pointer:StatePtr) event on_transition =
            match rec_trigger (pointer.pointing_in states) event with
            | TransitionFailure error -> failure_flat event pointer error
            | TransitionTo i -> on_transition i
            | NoTransition -> ActionSuccess        
        
        let rec transfer states (pointer:StatePtr) destination =        
            match rec_trigger (pointer.pointing_in states) OnExit with
            | TransitionFailure error -> failure_flat OnExit pointer error
            | TransitionTo i -> failure_transition_on_exit pointer i
            | NoTransition -> pointer.index <- destination; trigger_flat states pointer OnEnter (transfer states pointer)
                
        let trigger_flat_default states pointer event = trigger_flat states pointer event (transfer states pointer)
                
        let trigger_nested states initial (pointer:StatePtr) =
            match event with
            | OnEnter -> pointer.index <- initial; trigger_flat_default states pointer OnEnter
            | OnExit -> trigger_flat states pointer OnExit (failure_transition_on_exit pointer)
            | _ -> trigger_flat_default states pointer event
        
        let trigger_actions actions event =
            let rec rec_trigger = function
                | action:Action :: tail ->
                    match action event with
                    | ActionFailure _ as failure -> failure
                    | ActionSuccess -> rec_trigger tail
                | [] -> ActionSuccess
            rec_trigger actions
            
        let test_transitions transitions event =
            let rec rec_test = function
                | transition:Transition :: tail ->
                    match transition event with
                    | TransitionFailure _ as failure -> failure
                    | TransitionTo _ as transition_to -> transition_to
                    | NoTransition -> rec_test tail
                | [] -> NoTransition
            rec_test transitions
        
        match state with
        | Flat(actions, transitions) -> 
            match trigger_actions actions event with
            | ActionFailure message -> error_deep message
            | ActionSuccess -> test_transitions transitions event
        | Super(states, initial, pointer, transitions) ->                        
            match trigger_nested states initial pointer with
            | ActionFailure message -> error_deep message
            | ActionSuccess -> test_transitions transitions event
            
    match rec_trigger state event with
    | TransitionFailure message -> ActionFailure message
    | TransitionTo _ -> ActionFailure "transition from root state"
    | NoTransition -> ActionSuccess
    

module action =
    let on_enter action = function
        | OnEnter -> action ()
        | _ -> ActionSuccess
        
    let on_key_down key action = function
        | OnKeyDown k -> if k = key then action () else ActionSuccess
        | _ -> ActionSuccess

    let on_key_up key action = function
        | OnKeyUp k -> if k = key then action () else ActionSuccess
        | _ -> ActionSuccess
        
module transition =    
    let on_enter destination = function
        | OnEnter -> TransitionTo destination
        | _ -> NoTransition
        
    let on_key_down key destination = function
        | OnKeyDown k -> if k = key then TransitionTo destination else NoTransition
        | _ -> NoTransition
        
    let on_key_up key destination = function
        | OnKeyUp k -> if k = key then TransitionTo destination else NoTransition
        | _ -> NoTransition
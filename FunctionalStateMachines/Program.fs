open states

[<EntryPoint>]
let main argv =
    
    let print text = fun () -> printfn $"%s{text}"; ActionSuccess
    
    let print_on_space text = action.on_key_down "space" (print text)    
    
    let machine = make_root 0 [|
        make_super_state 0 [|
            make_flat_state [ print_on_space "_0_0" ] [ transition.on_key_up "1" 1 ]
            make_super_state 0 [|
                make_flat_state [ print_on_space "_0_1_init" ] [ transition.on_enter  1 ]
                make_flat_state [ print_on_space "_0_1_0" ] [ transition.on_key_up "6" 2 ]
                make_flat_state [ print_on_space "_0_1_1" ] [ transition.on_key_up "7" 1 ]
            |] [ transition.on_key_up "0" 0 ]
        |] [ transition.on_key_down "1" 1 ; transition.on_key_down "2" 2 ]
        make_flat_state [ print_on_space "_1" ] [ transition.on_key_down "0" 0 ; transition.on_key_down "2" 2 ]
        make_flat_state [ print_on_space "_2" ] [ transition.on_key_down "0" 0 ; transition.on_key_down "1" 1 ]
    |]
    
    let trigger event =
        match trigger_event machine event with
        | ActionFailure message -> printfn $"%s{message}"
        | ActionSuccess -> ()
        
    let trigger_down key = trigger (OnKeyDown key)
    let trigger_up key = trigger (OnKeyUp key)
    
    trigger_down "space"
    trigger_up "1"
    trigger_down "space"
    trigger_up "6"
    trigger_down "space"
    trigger_up "7"
    trigger_down "space"
    trigger_down "1"
    trigger_down "space"
    trigger_down "space"
    trigger_down "2"
    trigger_down "space"
    trigger_down "1"
    trigger_down "space"
    trigger_up "1"
    trigger_down "0"
    trigger_down "space"
    trigger_up "1"
    trigger_down "space"
    
    0
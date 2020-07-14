namespace NFA
open DFA

type NFA<'Q, 'A when 'Q: equality and 'A: equality> (delta: List<'Q*Option<'A>*'Q>, q0: 'Q, f: List<'Q>) =
    member this.ReadAlphabetList str =
        let applyAlphabet a q =
            let condition q a (q0, a1, _) =
                match a1 with
                    | Some(x) -> (q = q0) && (a = x)
                    | None -> q = q0
            List.filter (condition q a) delta
            |> List.map (fun (_, _, x) -> x)
        let rec apply s q =
            // printfn "running: %A %A" q str  // Debug print
            match s with
                | head :: tail ->
                    List.map (applyAlphabet head) q
                    |> List.fold List.append [] 
                    |> apply tail
                | [] -> q
        apply str [q0]

    member this.IsAccept str =
        this.ReadAlphabetList str 
        |> List.map (fun x -> List.contains x f)
        |> List.contains true

let NFAtoDFA<'Q, 'A when 'Q: (member NewState<'T> : 'T -> 'Q<'T>)> nfa =
    

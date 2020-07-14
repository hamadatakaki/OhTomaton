namespace DFA

type DFA<'Q, 'A when 'Q: equality and 'A: equality> (delta: List<'Q*'A*'Q>, q0: 'Q, f: List<'Q>) =
    member this.ReadAlphabetList str = 
        let applyAlphabet a q =
            let condition q a (q0, a1, _) = (q = q0) && (a = a1)
            List.filter (condition q a) delta
            |> List.map (fun (_, _, x) -> x)
        let rec apply s q = 
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

namespace NFA
open DFA

type NFA<'Q, 'A when 'Q : comparison and 'A : comparison> (delta: Set<'Q*Option<'A>*'Q>, s: 'Q, f: Set<'Q>) =
    let rec epsilonSkip (qs: Set<'Q>) =
        let unitEpsilonTrans (q: 'Q) =
            let cond (q0, a1, _) = (q0 = q) && (a1 = None)
            Set.filter cond delta
            |> Set.map (fun (_, _, x) -> x)

        if Set.isEmpty qs then
            qs
        else
            Set.map unitEpsilonTrans qs
            |> Set.unionMany
            |> epsilonSkip
            |> Set.union qs

    let firstEpsilonTrans (q: 'Q) = set[q] |> epsilonSkip

    // \cup_{x \in Q} T(x, a)
    let alphabetTrans (a: 'A) (qs: Set<'Q>) =
        let unitTrans (a: 'A) (q: 'Q) =
            let condition (q0, a1, _) =
                match a1 with
                    | Some(x) -> (q = q0) && (a = x)
                    | None -> q = q0
            Set.filter condition delta
            |> Set.map (fun (_, _, x) -> x)

        Set.map (unitTrans a) qs
        |> Set.unionMany
        |> epsilonSkip

    member this.ReadAlphabetSeq (str: List<'A>) =
        let rec apply (alpha: List<'A>) (qs: Set<'Q>) =
            match alpha with
                | head :: tail ->
                    alphabetTrans head qs
                    |> apply tail
                | [] -> qs

        firstEpsilonTrans s |> apply str 

    member this.IsAccept str =
        this.ReadAlphabetSeq str 
        |> Set.map (fun x -> Set.contains x f)
        |> Set.contains true

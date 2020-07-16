namespace OhTomaton

open DFA

module NFA =
    type NT<'Q, 'A> = 'Q*Option<'A>*'Q

    type NFA<'Q, 'A when 'Q : comparison and 'A : comparison> (delta: Set<NT<'Q, 'A>>, s: 'Q, f: Set<'Q>) =
        let unitEpsilonTrans (q: 'Q) =
                let cond (q0, a1, _) = (q0 = q) && (a1 = None)
                Set.filter cond delta
                |> Set.map (fun (_, _, x) -> x)

        let rec epsilonSkip (qs: Set<'Q>) =
            if Set.isEmpty qs then
                qs
            else
                Set.map unitEpsilonTrans qs
                |> Set.unionMany
                |> epsilonSkip
                |> Set.union qs

        let firstEpsilonTrans (q: 'Q) = unitEpsilonTrans q |> epsilonSkip

        // \cup_{x \in Q} T(x, a)
        let alphabetTrans (a: 'A) (qs: Set<'Q>) : Set<'Q> =
            let unitTrans (a: 'A) (q: 'Q) =
                let condition (q0, a1, _) =
                    match a1 with
                        | Some(x) -> (q = q0) && (a = x)
                        | None -> false
                Set.filter condition delta
                |> Set.map (fun (_, _, x) -> x)

            Set.map (unitTrans a) qs
            |> Set.unionMany
            |> epsilonSkip

        let calcTransition (a: 'A) (qs: Set<'Q>) =
            (qs, a, alphabetTrans a qs)

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

        member this.GetSigma =
            let optionToSet (a: Option<'a>) =
                match a with
                    | Some(x) -> set[x]
                    | None -> set[]
            Set.map (fun (_, a, _) -> optionToSet a) delta |> Set.unionMany

        member this.ConvertToDFA : DFA.DFA<Set<'Q>, 'A> =
            let q0 = firstEpsilonTrans s
            let mutable investigated = set[]
            let rec calcDelta (qs: Set<'Q>): Set<DT<Set<'Q>, 'A>> =
                if qs.IsEmpty then
                    set[]
                else
                    let subDelta = Set.map (fun a -> calcTransition a qs) this.GetSigma |> Set.filter (fun (_, _, y) -> not y.IsEmpty)
                    let start = Set.map (fun (y, _, _) -> y) subDelta
                    let finish = Set.map (fun (_, _, y) -> y) subDelta
                    investigated <- Set.union investigated start
                    let appliedDelta = Set.map calcDelta (Set.difference finish investigated)
                    Set.unionMany appliedDelta |> Set.union subDelta

            let newDelta = calcDelta q0
            let newF =
                Set.map (fun x -> Set.filter (fun y -> Set.contains x y) investigated) f
                |> Set.unionMany
            DFA.DFA(newDelta, q0, newF)

        override this.ToString() = sprintf "<%A, %A, %A>" delta s f

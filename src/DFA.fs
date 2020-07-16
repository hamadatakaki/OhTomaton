namespace OhTomaton

module DFA =
    type DT<'Q, 'A> = 'Q*'A*'Q

    type DFA<'Q, 'A when 'Q : comparison and 'A : comparison> (delta: Set<DT<'Q, 'A>>, s: 'Q, f: Set<'Q>) =
        member this.ReadAlphabetSeq (str: List<'A>) =
            let applyAlphabet a q =
                let condition (q0, a1, _) = (q = q0) && (a = a1)
                Set.filter condition delta
                |> Set.map (fun (_, _, x) -> x)

            let rec apply (alpha: List<'A>) (qs: Set<'Q>) =
                match alpha with
                    | head :: tail ->
                        Set.map (applyAlphabet head) qs
                        |> Set.unionMany
                        |> apply tail
                    | [] -> qs

            apply str (set[s])

        member this.IsAccept str =
            this.ReadAlphabetSeq str
            |> Set.map (fun x -> Set.contains x f)
            |> Set.contains true

        override this.ToString() = sprintf "<%A, %A, %A>" delta s f

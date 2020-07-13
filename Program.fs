// Learn more about F# at http://fsharp.org

open System

module OhTomaton =
    type DFA<'Q, 'A when 'Q: equality> (delta: 'Q -> 'A -> Option<'Q>, q0: 'Q, f: List<'Q>) =
        member this.ReadAlphabet a = 
            match delta q0 a with
                | Some(S) -> S
                | None -> 
                    printfn "Exception: state is %A, alphabet is %A" q0 a
                    Operators.exit(1)

        member this.ReadAlphabetList str = 
            let f q head =
                match delta q head with 
                    | Some(x) -> x
                    | None -> 
                        printfn "Exception: state is %A, alphabet is %A" q head
                        Operators.exit(1)
            let rec apply q str = 
                match str with
                    | head :: tail -> 
                        let next = f q head
                        // printfn "from: %A, to: %A" q next  // Debug print
                        apply next tail
                    | [] -> q
            apply q0 str

        member this.IsAccept str =
            List.contains (this.ReadAlphabetList str) f

    type NFA<'Q, 'A when 'Q: equality and 'A: equality> (delta: List<'Q*Option<'A>*'Q>, q0: 'Q, f: List<'Q>) =
        member this.ReadAlphabetList str =
            let rec apply str q =
                // printfn "running: %A %A" q str  // Debug print
                let applyAlphabet a q =
                    let condition q a (q0, a1, _) =
                        match a1 with
                            | Some(x) -> (q = q0) && (a = x)
                            | None -> q = q0
                    let extract (_, _, q1) = q1
                    List.filter (condition q a) delta 
                    |> List.map extract
                match str with
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

    [<CustomEquality; NoComparison>]
    type State =
        | S0
        | S1
        | S2
        | S3
        | S4
        | S5
        | S6
        | S7

        static member AllState = [ 
            yield S0
            yield S1
            yield S2
            yield S3
            yield S4
            yield S5
            yield S6
            yield S7
        ]

        static member ToInt s =
            match s with
                | S0 -> 0
                | S1 -> 1
                | S2 -> 2
                | S3 -> 3
                | S4 -> 4
                | S5 -> 5
                | S6 -> 6
                | S7 -> 7

        interface System.IEquatable<State> with
            member this.Equals(other) = State.ToInt this = State.ToInt other

    [<CustomEquality; NoComparison>]
    type Alphabet =
        | A
        | B

        static member AllAlphabet = [
            yield A
            yield B
        ]

        static member FromChar c =
            match c with
            | 'a' -> Some(A)
            | 'b' -> Some(B)
            | _ -> None

        static member ToInt a =
            match a with
                | A -> 1
                | B -> 2

        static member FromString (str: string) =
            let pushAcc (acc: List<Alphabet>) (o: Option<Alphabet>) =
                match o with
                | Some(a) -> List.append acc [a]
                | None -> acc
            Seq.toList str
            |> List.fold (fun acc c -> pushAcc acc (Alphabet.FromChar c)) []

        interface System.IEquatable<Alphabet> with
            member this.Equals(other) = Alphabet.ToInt this = Alphabet.ToInt other

    let Q = State.AllState
    let Sigma = Alphabet.AllAlphabet
    // let transition q a =
    //     match (q, a) with
    //     | (State.S0, Alphabet.A) -> Some(State.S1)
    //     | (State.S1, Alphabet.B) | (State.S3, Alphabet.B) | (State.S8, Alphabet.B) -> Some(State.S2)
    //     | (State.S2, Alphabet.A) -> Some(State.S3)
    //     | (State.S0, Alphabet.C) | (State.S2, Alphabet.C) | (State.S4, Alphabet.C) -> Some(State.S4)
    //     | (State.S4, Alphabet.B) -> Some(State.S5)
    //     | (State.S5, Alphabet.C) | (State.S7, Alphabet.C) -> Some(State.S6)
    //     | (State.S6, Alphabet.B) -> Some(State.S7)
    //     | (State.S4, Alphabet.A) | (State.S6, Alphabet.A) | (State.S8, Alphabet.A) -> Some(State.S8)
    //     | _ -> None // wrong transition
    let transitionList = [
        (State.S0, None, State.S1);
        (State.S1, None, State.S2);
        (State.S1, None, State.S4);
        (State.S1, None, State.S7);
        (State.S2, Some(Alphabet.A), State.S3);
        (State.S3, None, State.S6);
        (State.S4, Some(Alphabet.B), State.S5);
        (State.S5, None, State.S6);
        (State.S6, None, State.S1);
    ]

    let F = [State.S7]

    // let Dfa = DFA(transition, State.S0, F)

    let Nfa = NFA(transitionList, State.S0, F)

    let s = "ababaaabbb"
    s |> Alphabet.FromString
    |> Nfa.IsAccept
    |> printfn "Is '%s' accept?:  %b" s

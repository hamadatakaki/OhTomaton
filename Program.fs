// Learn more about F# at http://fsharp.org

open System

module OhTomaton =
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
        | S8

        static member AllState = [ 
            yield S0
            yield S1 
            yield S2
            yield S3 
            yield S4 
            yield S5 
            yield S6 
            yield S7 
            yield S8
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
                | S8 -> 8

        interface System.IEquatable<State> with
            member this.Equals(other) = State.ToInt this = State.ToInt other

    type Alphabet =
        | A
        | B
        | C

        static member AllAlphabet = [
            yield A
            yield B
            yield C
        ]

        static member FromChar (c: char) =
            match c with
            | 'a' -> Some(A)
            | 'b' -> Some(B)
            | 'c' -> Some(C)
            | _ -> None

        static member FromString (str: string) =
            let pushAcc (acc: List<Alphabet>) (o: Option<Alphabet>) =
                match o with
                | Some(a) -> List.append acc [a]
                | None -> acc
            Seq.toList str
            |> List.fold (fun acc c -> pushAcc acc (Alphabet.FromChar c)) []

    type DFA<'Q, 'A when 'Q: equality> (q: List<'Q>, sigma: List<'A>, delta: 'Q -> 'A -> Option<'Q>, q0: 'Q, f: List<'Q>) =
        member this.ReadAlphabet (a: 'A) = 
            match delta q0 a with
                | Some(S) -> S
                | None -> 
                    printfn "Exception: state is %A, alphabet is %A" q0 a
                    Operators.exit(1)

        member this.ReadAlphabetList (str: List<'A>) = 
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
                        printfn "from: %A, to: %A" q next
                        apply next tail
                    | [] -> q
            apply q0 str

        member this.IsAccept (str: List<'A>) =
            List.contains (this.ReadAlphabetList str) f

    let Q = State.AllState
    let Sigma = Alphabet.AllAlphabet
    let transition q a =
        match (q, a) with
        | (State.S0, Alphabet.A) -> Some(State.S1)
        | (State.S1, Alphabet.B) | (State.S3, Alphabet.B) | (State.S8, Alphabet.B) -> Some(State.S2)
        | (State.S2, Alphabet.A) -> Some(State.S3)
        | (State.S0, Alphabet.C) | (State.S2, Alphabet.C) | (State.S4, Alphabet.C) -> Some(State.S4)
        | (State.S4, Alphabet.B) -> Some(State.S5)
        | (State.S5, Alphabet.C) | (State.S7, Alphabet.C) -> Some(State.S6)
        | (State.S6, Alphabet.B) -> Some(State.S7)
        | (State.S4, Alphabet.A) | (State.S6, Alphabet.A) | (State.S8, Alphabet.A) -> Some(State.S8)
        | _ -> None // wrong transition
    let F = [State.S4; State.S6; State.S8]

    let Automaton = DFA(Q, Sigma, transition, State.S0, F)

    // case true
    Alphabet.FromString "abccababcabcbcabcbcaa"
    |> Automaton.IsAccept
    |> printfn "%b"

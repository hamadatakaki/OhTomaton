// Learn more about F# at http://fsharp.org

open System

module OhTomaton =
    [<CustomEquality; NoComparison>]
    type State =
        | S0
        | S1
        | S2

        static member AllState = [ 
            yield S0
            yield S1 
            yield S2
        ]

        static member ToInt s =
            match s with
                | S0 -> 0
                | S1 -> 1
                | S2 -> 2

        interface System.IEquatable<State> with
            member this.Equals(other) = State.ToInt this = State.ToInt other

    type Alphabet =
        | A
        | B

        static member AllAlphabet = [
            yield A
            yield B
        ]

        static member FromChar (c: char) =
            match c with
            | 'A' -> Some(A)
            | 'B' -> Some(B)
            | _ -> None

        static member FromString (str: string) =
            let pushAcc (acc: List<Alphabet>) (o: Option<Alphabet>) =
                match o with
                | Some(a) -> List.append acc [a]
                | None -> acc
            Seq.toList str
            |> List.fold (fun acc c -> pushAcc acc (Alphabet.FromChar c)) []

    let transition q a =
        match (q, a) with
        | (State.S1, Alphabet.B) | (State.S2, Alphabet.A) -> State.S0
        | (State.S0, Alphabet.A) | (State.S2, Alphabet.B) -> State.S1
        | _ -> State.S2

    type DFA<'Q, 'A when 'Q: equality> (q: List<'Q>, sigma: List<'A>, delta: 'Q -> 'A -> 'Q, q0: 'Q, f: List<'Q>) =
        member this.ReadAlphabet (a: 'A) = delta q0 a

        member this.ReadAlphabetList (str: List<'A>) = 
            let rec applyDelta q str = 
                match str with
                | head :: tail -> applyDelta (delta q head) tail
                | [] -> q
            applyDelta q0 str

        member this.IsAccept (str: List<'A>) =
            List.contains (this.ReadAlphabetList str) f

    let Q = State.AllState
    let Sigma = Alphabet.AllAlphabet
    let F = [State.S2]

    let Automaton = DFA(Q, Sigma, transition, State.S0, F)

    Automaton.ReadAlphabet Alphabet.A
    |> State.ToInt
    |> printfn "%d"

    Alphabet.FromString "AABAAAA"
    |> Automaton.ReadAlphabetList
    |> State.ToInt
    |> printfn "%d"

    // case true
    Alphabet.FromString "AABAAAA"
    |> Automaton.IsAccept
    |> printfn "%b"

    Alphabet.FromString "AABAAAA"
    |> Automaton.IsAccept
    |> printfn "%b"

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

    let transition q a =
        match (q, a) with
        | (State.S1, Alphabet.B) | (State.S2, Alphabet.A) -> State.S0
        | (State.S0, Alphabet.A) | (State.S2, Alphabet.B) -> State.S1
        | _ -> State.S2

    type DFA<'Q, 'A when 'Q: equality>(q: List<'Q>, sigma: List<'A>, delta: 'Q -> 'A -> 'Q, q0: 'Q, f: List<'Q>) =
        member this.ReadAlphabet (a: 'A) = delta q0 a

        member this.ReadString (str: List<'A>) = 
            let rec applyDelta q str = 
                match str with
                | head :: tail -> applyDelta (delta q head) tail
                | [] -> q
            applyDelta q0 str

        member this.IsAccept (str: List<'A>) =
            List.contains (this.ReadString str) f

    let Q = State.AllState
    let Sigma = Alphabet.AllAlphabet
    let F = [State.S2]

    let Automaton = DFA(Q, Sigma, transition, State.S0, F)

    Automaton.ReadAlphabet Alphabet.A
    |> State.ToInt
    |> printfn "%d"
    
    Automaton.ReadString [Alphabet.A; Alphabet.A; Alphabet.B; Alphabet.A; Alphabet.A; Alphabet.A; Alphabet.A]
    |> State.ToInt
    |> printfn "%d"

    // case true
    Automaton.IsAccept [Alphabet.A; Alphabet.A; Alphabet.B; Alphabet.A; Alphabet.A; Alphabet.A; Alphabet.A]
    |> printfn "%b"

    // case false
    Automaton.IsAccept [Alphabet.A; Alphabet.A; Alphabet.A; Alphabet.A]
    |> printfn "%b"

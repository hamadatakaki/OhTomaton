// Learn more about F# at http://fsharp.org

open System

module OhTomaton =
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

    type DFA<'Q, 'A> (q: List<'Q>, sigma: List<'A>, delta: 'Q -> 'A -> 'Q, q0: 'Q, f: List<'Q>) =
        member this.ReadCharacter(a: 'A) = delta q0 a

    let Q = State.AllState
    let Sigma = Alphabet.AllAlphabet
    let F = [State.S2]

    let Automaton = DFA(Q, Sigma, transition, State.S0, F)

    Automaton.ReadCharacter Alphabet.A
    |> State.ToInt
    |> printfn "%d"

[<EntryPoint>]
let main argv =
    0

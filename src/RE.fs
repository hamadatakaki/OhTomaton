namespace OhTomaton

open NFA

module RE =
    type NFA = NFA<int, char>
    type NT = NT<int, char>
    exception SyntaxException of string

    let nfaFromChar (c: char) (s: int) =
        let delta = set[(s, Some(c), s+1)]
        NFA(delta, s, set[s+1])

    let nfaFromEpsilon (s: int) =
        let delta = set[(s, None, s+1)]
        NFA(delta, s, set[s+1])

    let nfaFromUnion (s: int) (nfa1: NFA) (nfa2: NFA) =
        let delta =
            set[(s, None, nfa1.S0); (s, None, nfa2.S0)]
            |> Set.union (Set.map (fun f -> (f, None, s+1)) nfa1.F)
            |> Set.union (Set.map (fun f -> (f, None, s+1)) nfa2.F)
            |> Set.union nfa1.Delta
            |> Set.union nfa2.Delta
        NFA(delta, s, set[s+1])

    let nfaFromStar (s: int) (nfa: NFA) =
        let delta =
            set[(s, None, nfa.S0); (nfa.S0, None, s+1)]
            |> Set.union (Set.map (fun f -> (f, None, nfa.S0)) nfa.F)
            |> Set.union nfa.Delta
        NFA(delta, s, set[s+1])

    type Token =
        | Character of char
        | LParen
        | RParen
        | Union
        | Star
        | Dollar

        static member FromAlphabet (c: char): Token =
            match c with
                | '(' -> LParen
                | ')' -> RParen
                | '|' -> Union
                | '*' -> Star
                | c -> Character(c)

        static member FromAlphabetSeq (str: List<char>): List<Token> =
            let seq = List.map Token.FromAlphabet str
            seq @ [Dollar]


    type Node =
        | Character of char
        | Dollar
        | Epsilon
        | Concat of (Node * Node)
        | Union of (Node * Node)
        | Star of Node

        member this.ToNFA (state: int) =
            match this with
                | Character(c) -> (nfaFromChar c state, state+2)
                | Epsilon -> (nfaFromEpsilon state, state+2)
                | Concat(r1, r2) ->
                    let (nfa1, newState) = r1.ToNFA(state)
                    let (nfa2, newState) = r2.ToNFA(newState)
                    (nfa1+nfa2, newState+2)
                | Union(r1, r2) ->
                    let (nfa1, newState) = r1.ToNFA(state)
                    let (nfa2, newState) = r2.ToNFA(newState)
                    (nfaFromUnion newState nfa1 nfa2, newState+2)
                | Star(r) ->
                    let (nfa, newState) = r.ToNFA(state)
                    (nfaFromStar newState nfa, newState+2)
                | Dollar -> raise (SyntaxException("@ Sequence : the Sequence has unnecessary Star."))

    type Parser (tokens: List<Token>) =
        let mutable look = 0

        member this.LookAt = tokens.Item(look)

        member this.Check (token: Token) = this.LookAt = token

        member this.Forward = look <- look + 1

        member this.Expr =
            let union = this.Union
            assert (this.Check(Token.Dollar))
            union

        member this.Union =
            let sequence = this.Sequence
            match this.LookAt with
                | Token.Union ->
                    this.Forward
                    Union(sequence, this.Union)
                | _ -> sequence

        member this.Sequence =
            match this.LookAt with
                | Token.LParen ->
                    let star = this.Star
                    let sequence = this.Sequence
                    if sequence = Node.Epsilon then
                        star
                    else
                        Concat(star, sequence)
                | Token.Character(c) ->
                    let star = this.Star
                    let sequence = this.Sequence
                    if sequence = Node.Epsilon then
                        star
                    else
                        Concat(star, sequence)
                | Token.Dollar | Token.RParen | Token.Union -> Epsilon
                | _ -> raise (SyntaxException("@ Sequence : the Sequence has unnecessary Star."))

        member this.Star =
            let factor = this.Factor
            if this.LookAt = Token.Star then
                this.Forward
                Star(factor)
            else
                factor

        member this.Factor =
            let mutable node = Epsilon
            if this.Check(Token.LParen) then
                this.Forward
                node <- this.Union
                assert (this.Check(Token.RParen))
                this.Forward
            else
                match this.LookAt with
                    | Token.Character(c) -> 
                        node <- Character(c)
                        this.Forward
                    | _ -> raise (SyntaxException("@ Factor : the Factor doesn't start with '(' and is not Character."))
            node

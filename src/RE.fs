namespace OhTomaton

open NFA

module RE =
    type NFA = NFA<int, char>

    type Token =
        | Character of char
        | LParen
        | RParen
        | Union
        | Star
        | EOF

        static member FromAlphabet (c: char): Token =
            match c with
                | '(' -> LParen
                | ')' -> RParen
                | '|' -> Union
                | '*' -> Star
                | c -> Character(c)

        static member FromAlphabetSeq (str: List<char>): List<Token> =
            let seq = List.map Token.FromAlphabet str
            seq @ [EOF]

    type Node =
        | Character of char
        | EOF
        | Epsilon
        | Concat of (Node * Node)
        | Union of (Node * Node)
        | Star of Node

    exception SyntaxException of string

    type Parser (tokens: List<Token>) =
        let mutable look = 0

        member this.LookAt = tokens.Item(look)

        member this.Check (token: Token) = this.LookAt = token

        member this.Forward = look <- look + 1

        member this.Expr =
            let union = this.Union
            assert (this.Check(Token.EOF))
            Concat(union, EOF)

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
                | Token.EOF | Token.RParen | Token.Union -> Epsilon
                | _ -> raise (SyntaxException("hoge"))

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
                    | _ -> raise (SyntaxException("hoge"))
            node

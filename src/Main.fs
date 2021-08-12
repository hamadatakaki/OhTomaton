// Learn more about F# at http://fsharp.org

namespace OhTomaton

open NFA
open RE

module Main =
    let transitionSet = set [
        (0, None, 1);
        (1, None, 2);
        (1, None, 4);
        (1, None, 7);
        (2, Some('a'), 3);
        (3, None, 6);
        (4, Some('b'), 5);
        (5, None, 6);
        (6, None, 1);
        (7, None, 8);
        (8, Some('a'), 9);
        (9, None, 10);
        (10, Some('b'), 11);
    ]

    let F = set [11]

    let Nfa = NFA<int, char>(transitionSet, 0, F)

    let s = "abc"

    let regexParser = 
        [for c in "(a|b|c|(ab)*)*abc*" -> c]
        |> Token.FromAlphabetSeq
        |> Parser

    let Nfa2 = regexParser.ToNFA
    [for c in s -> c]
    |> Nfa2.IsAccept
    |> printfn "NFA from RE: Is '%s' accept?:  %b" s

    let Dfa2 = Nfa2.ConvertToDFA

    [for c in s -> c]
    |> Dfa2.IsAccept
    |> printfn "DFA from RE: Is '%s' accept?:  %b" s

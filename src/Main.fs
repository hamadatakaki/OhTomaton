// Learn more about F# at http://fsharp.org

namespace OhTomaton

open NFA

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

    let Nfa = NFA.NFA<int, char>(transitionSet, 0, F)

    let s = "aaaabbbbbbabaabababababbababbbbbaaab"
    [for c in s -> c]
    |> Nfa.IsAccept
    |> printfn "Is '%s' accept?:  %b" s

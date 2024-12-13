app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}
import pf.Stdout

main =
    Set.fromList [N, S, W, N, S, N, S, N, E, S]
    |> solve2
    |> Inspect.toStr
    |> Stdout.line!

solve2 : Set [N, E, S, W] -> List (Set [N, E, S, W])
solve2 = \input ->
    f = \set ->
        when
            when Set.walkUntil set (Err Empty) \_, a -> Break (Ok a) is
                Ok a -> Ok (a, Set.remove set a)
                _ -> Err Empty
        is
            Ok (fence, ) ->
                f (Set.difference set (Set.single fence))

            Err Empty -> []

    f input

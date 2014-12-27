import Data.List (sort)

tenPowers :: (Integral a) => [a]
tenPowers = [10 ^ x | x <- [1..]]

digits n = reverse [div (mod n x) (div x 10) | x <- takeWhile (<= (n * 10)) tenPowers]
bouncy l = let s = sort(l)
           in not (l == s || reverse l == s)

tally :: (Integral a) => [Bool] -> (a, a)
tally l = foldl (\acc b -> if b
                           then (1 + fst acc, 1 + snd acc)
                           else (fst acc,     1 + snd acc)) (0, 0) l

firstAtProportion p = let inner [] _ = error "Proprotion not reached!"
                          inner (n:ns) acc = let pos = fst acc
                                                 tot = snd acc
                                                 rat = pos / tot
                                             in if rat >= p
                                                then floor tot
                                                else if bouncy (digits n)
                                                     then inner ns (1 + pos, 1 + tot)
                                                     else inner ns (pos, 1 + tot)
                      in inner [1..] (0, 0)

main = putStrLn (show (firstAtProportion (99/100)))

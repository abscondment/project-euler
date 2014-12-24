rev n r
  | n == 0 = r
  | otherwise = rev (div n 10) (10 * r + (mod n 10))

tenPowers = [floor(10 ** x) | x <- [1..]]
digits n = reverse [div (mod n x) (div x 10) | x <- takeWhile (<= (n * 10)) tenPowers]
allOdd n = all (odd) (digits n)

revsUnder :: Int -> [Int]
revsUnder m = [n | n <- [1..m], mod n 10 > 0 && allOdd (n + (rev n 0))]

analytic :: Int -> Int
analytic n = case (mod n 4) of 0 -> 20 * floor (30 ** (fromIntegral ((div n 2) - 1)))
                               2 -> 20 * floor (30 ** (fromIntegral ((div n 2) - 1)))
                               1 -> 100 * floor (500 ** (fromIntegral ((div n 4) - 1)))
                               _ -> 0

solution = sum (map (analytic) [1..9])

-- main = putStrLn (show (length (revsUnder 1000000000)))
-- 0

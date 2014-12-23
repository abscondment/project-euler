isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

factors :: Int -> [Int]
factors x =
  let lower = [f | f <- [2..isqrt x], mod x f == 0]
      upper = [(div x ff) | ff <- lower]
  in lower ++ upper


primeFactors :: Int -> [Int]
primeFactors x = [f | f <- factors x, length(factors f) == 0]

solution = maximum (primeFactors 600851475143)

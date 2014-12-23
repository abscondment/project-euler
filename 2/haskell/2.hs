-- naive
fib 0 = 1
fib 1 = 1
fib x = fib(x - 1) + fib(x - 2)

-- a little better
fib2 n = fibs !! n

-- list - learned about zipWith!
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- infinite list, at that - learned about takeWhile!
solution = sum([x | x <- takeWhile (<= 4000000) fibs, mod x 2 == 0])

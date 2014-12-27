import System.IO
import Control.Monad

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list = first : splitBy f (dropWhile f rest) where
  (first, rest) = break f list

-- adapted from:
-- http://stackoverflow.com/questions/14550458/find-max-element-and-index-of-a-list-in-haskell
-- maxPow :: [[Integer]] -> Int -> (Integer, Int)
-- maxPow [] _   = error "Empty list"
-- maxPow [[]] _ = error "Empty list"
-- maxPow [p] idx = ((head p) ^ (last p), idx)
-- maxPow (p:ps) idx
--   | pv > tv = (pv, idx)
--   | otherwise = (tv, tidx)
--   where powf l = (head l) ^ (last l)
--         (tv, tidx) = maxPow ps (idx + 1)
--         pv = powf p


powSort x y = fromIntegral (floor (x * (log y)))

powList [] = error "Empty list"
powList (l:ls) = foldr (powSort) l ls

fastMaxPow :: (RealFrac a, Floating a) => [[a]] -> Int -> (a, Int)
fastMaxPow [] _   = error "Empty list"
fastMaxPow [[]] _ = error "Empty list"
fastMaxPow [p] idx = (powList p, idx)
fastMaxPow (p:ps) idx
  | pv > tv = (pv, idx)
  | otherwise = (tv, tidx)
  where (tv, tidx) = fastMaxPow ps (idx + 1)
        pv = powList p

getInts :: RealFrac b => String -> [[b]]
getInts c = ((map (map (fromInteger))) .
             (map (map (readInt))) .
             (map (splitBy (== ','))) .
             lines) c
  where
    readInt = read :: String -> Integer

calc fname = do
  contents <- readFile fname
  print $ fastMaxPow (getInts contents) 1

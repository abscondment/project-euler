import System.IO
import Control.Monad

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list = first : splitBy f (dropWhile f rest) where
  (first, rest) = break f list

readNodes c = ((map (map readNode)) .
               (map (splitBy (== ','))) .
               lines) c
  where
    readInt = read :: String -> Integer
    readNode s = if s == "-"
                 then -1
                 else read s

readGraph fname = do
  contents <- readFile fname
  print $ readNodes contents

import System.IO
import Control.Monad
import Data.Char (chr)
import Data.List (nub)
import Data.Graph.Inductive.Graph (Graph, Node, LEdge, LNode, mkGraph)
import Data.Graph.Inductive.Tree (Gr)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list = first : splitBy f (dropWhile f rest) where
  (first, rest) = break f list

withIndexes l = zip [1..] l

readEdges :: String -> [(Node, Node, Int)]
readEdges c = ((concatMap (\(a, ns) -> map (\(b, w) -> (a, b, w)) ns)) .
               withIndexes .
               (map (withIndexes)) .
               (map (filter (>= 0))) .
               (map (map (readNode))) .
               (map (splitBy (== ','))) .
               lines) c
  where
    readInt = read :: String -> Integer
    readNode s = if s == "-"
                 then -1
                 else read s

readGraph :: String -> Gr Char Int
readGraph c = let edges :: [LEdge Int]
                  edges = readEdges c
                  nodes :: [LNode Char]
                  nodes = zip
                          (nub (concatMap (\(a, b, w) -> [a, b]) edges))
                          (map (\c -> chr (c + 64)) [1..])
              in mkGraph nodes edges

load fname =
  do
    contents <- readFile fname
    print $ readGraph contents

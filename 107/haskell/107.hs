import System.IO
import Control.Monad
import Data.Char (chr)
import Data.List (nub, sort, sortBy)
import Data.Ord (comparing)
import Data.Graph.Inductive.Basic (undir)
import Data.Graph.Inductive.Graph (Graph, Node, LEdge, LPath(LP), LNode, emap, labEdges, mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.MST (msPath, msTree, msTreeAt)
import Data.Graph.Inductive.Internal.RootPath (getDistance)


splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list = first : splitBy f (dropWhile f rest) where
  (first, rest) = break f list

withIndexes l = zip [1..] l

readEdges :: String -> [(Node, Node, Int)]
readEdges c = (nub .
               (concatMap (\(a, ns) -> map (\(b, w) -> if b > a
                                                       then (a, b, w)
                                                       else (b, a, w)) ns)) .
               withIndexes .
               (map (filter (\(_, w) -> w >= 0))) .
               (map (withIndexes)) .
               (map (map (readNode))) .
               (map (splitBy (== ','))) .
               lines) c
  where
    readInt = read :: String -> Integer
    readNode s = if s == "-"
                 then -1
                 else read s

readGraph :: String -> Gr Int Int
readGraph c = let edges :: [LEdge Int]
                  edges = readEdges c
                  nodes :: [LNode Int]
                  nodes = zip
                          ((sort . nub . (concatMap (\(a, b, w) -> [a, b]))) edges)
                          -- (map (\c -> chr (c + 64)) [1..])
                          [1..]
              in undir (mkGraph nodes edges)

uweights gr = ((\s -> div s 2) . sum . (map (\(_, _, w) -> w)) . labEdges) gr

treeWeights tr = (sum .
                  (map snd) .
                  nub .
                  (concatMap (\(LP p)-> p))) tr

load fname =
  do
    contents <- readFile fname
    let graph = readGraph contents
        totWeight = uweights graph
        mst =  msTree graph
        minWeight = treeWeights mst
    print $ totWeight
    print $ minWeight
    print $ totWeight - minWeight

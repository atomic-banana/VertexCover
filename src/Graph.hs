module Graph (
    G(..)
    , Node
    , Edge
    , parseG
    , emptyG
    , checkG
    , filterAdjacentNode
    , howManyEdgeFor
) where

import qualified Data.Foldable      as F
import qualified Data.List          as L
import qualified Data.Tuple         as T

-- |A node type is an Int
type Node = Int

-- |An edge is a tuple of Int
type Edge = (Node, Node)

-- |A graph type
data G = G { getNVertices :: Int          -- the number of vertices
           , getNEdges    :: Int          -- the number of edges
           , getEdges     :: [Edge] -- the edges
           } deriving (Show)

-- |'emptyG' return an empty graph
emptyG :: G
emptyG = G { getNVertices = 0, getNEdges = 0, getEdges = [] }

-- |'checkG' 'g' returns 'True' if the graph 'g' is valid.
-- A graph is consider valid if (not sure that this is the case for PACE 2019)
--   1) the number of vertices matches the naximum vertex among the edges
--   2) the number of edges matches the number of stored edges
--   3) the graph does not contain duplicated edges
--   4) the graph does not contain self loops
checkG :: G -> Bool
checkG g = checkNVertices g && checkNEdges g && checkDuplicate g
  where
    checkNVertices g = checkNVerticesMax g && checkNVerticesMin g
    checkNVerticesMax g = getNVertices g == F.maximum (L.map (uncurry max) $ getEdges g)
    checkNVerticesMin g = 1 == F.minimum (L.map (uncurry min) $ getEdges g)
    checkNEdges    g = getNEdges g == L.length (getEdges g)
    checkDuplicate g = L.length (getEdges g) == L.length (L.nub $ getEdges g)

-- |'stringToInt' 's' converts the string 's' to integer
stringToInt :: String -> Int
stringToInt s = read s :: Int

-- Convenience function.
whenMaybe :: a -> Bool -> Maybe a
whenMaybe _ False = Nothing
whenMaybe a True  = Just a

-- |'parseG' 's' parses a graph given as a PACE 19 format list of strings.
-- The function returns 'Nothing' in case of parse error.
parseG :: [String] -> Maybe G
parseG = go emptyG . L.map L.words . L.filter (not . L.isPrefixOf "c")
  where
    go g []                    = whenMaybe g (checkG g)
    go g (["p","td",n,m] : ls) = go (g { getNVertices = stringToInt n, getNEdges = stringToInt m }) ls
    go g ([i,j]          : ls)
      | i == j                 = go (g { getEdges = getEdges g, getNEdges = getNEdges g -1 }) ls
      | i < j                  = go (g { getEdges = (stringToInt i, stringToInt j) : getEdges g  }) ls
      | otherwise              = go (g { getEdges = (stringToInt j, stringToInt i) : getEdges g  }) ls
    go _ _                     = Nothing

-- |'filterAdjacentNode' 'edges' 'current' 'node'
-- Filter all edges and extract the node which is connected to the other node.
-- e.g : filterAdjacentNode [(1, 2), (1, 3), (4, 1), (3, 4), (5, 8)] 1 []
--       -> [2, 3, 4] because there is 1 -> 2, 1 -> 3 and 1 <- 4
-- Add all extracted nodes to another list.
filterAdjacentNode :: [Edge] -> Node -> [Node] -> [Node]
filterAdjacentNode (x:xs) node nodes
  | fst x == node = filterAdjacentNode xs node $ snd x : nodes -- 'node' is the 'source'
  | snd x == node = filterAdjacentNode xs node $ fst x : nodes -- 'node' is the 'destination'
  | otherwise = filterAdjacentNode xs node nodes -- not concerned
filterAdjacentNode [] _ nodes = nodes

-- |'howManyEdgeFor' 'graph' 'node'
-- Count the number of edges associates with this node.
howManyEdgeFor :: G -> Node -> Int
howManyEdgeFor g n = sum [1 | (x, y) <- getEdges g, x == n || y == n]

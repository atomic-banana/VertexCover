module VertexCover (
    VC(..)
    , emptyVertexCover
    , select
    , writeVCTo
    , putInSolution
    , putOthersInSolution
) where

import qualified Data.List  as L
import qualified Graph      as G

-- |A vertex cover solution
data VC = VC { getVertices :: [Int] }
  deriving (Show)

-- |'emptyG' return an empty graph
emptyVertexCover :: VC
emptyVertexCover = VC { getVertices = [] }

-- |'toPACEFormat' 'vc'
-- Convert all nodes into the vertex cover solution into a string
-- which is compatible with PACE format.
-- See https://pacechallenge.org/2019/vc/vc_format/.
toPACEFormat :: VC -> String
toPACEFormat vc = L.intercalate "\n" . map show $ getVertices vc

-- |'writeVCTo' 'graph' 'vc' 'file path'
-- Write the given solution into the file.
writeVCTo :: G.G -> VC -> String -> IO()
writeVCTo graph vc path =
  writeFile path $ "s vc " ++
  show ( G.getNVertices graph ) ++
  ' ' :
  show ( length (getVertices vc) ) ++
  '\n' :
  toPACEFormat vc

-- |'select' 'v' select a vertices to put to the solution.
-- return a new graph without the selected vertices.
select :: G.G -> G.Node -> G.G
select (G.G 0 _ _)  v = error "Cannot use an empty graph"
select g            v = G.G {
    G.getNVertices = G.getNVertices g - 1
  , G.getNEdges    = length edges
  , G.getEdges     = edges
  }
  where
    edges = filter (\x -> fst x /= v && snd x /= v) $ G.getEdges g

-- |'filterAdjacentNode' 'edges' 'current' 'node'
-- Filter all edges and extract the node which is connected to the other node.
-- e.g : filterAdjacentNode [(1, 2), (1, 3), (4, 1), (3, 4), (5, 8)] 1
--       -> [2, 3, 4] because there is 1 -> 2, 1 -> 3 and 1 <- 4
-- Add all extracted nodes to another list.
filterAdjacentNode :: [G.Edge] -> G.Node -> [G.Node] -> [G.Node]
filterAdjacentNode (x:xs) node nodes
  | fst x == node = filterAdjacentNode xs node $ snd x : nodes -- 'node' is the 'source'
  | snd x == node = filterAdjacentNode xs node $ fst x : nodes -- 'node' is the 'destination'
  | otherwise = filterAdjacentNode xs node nodes -- not concerned
filterAdjacentNode [] _ nodes = nodes

-- |'putInSolution' '(graph, vc instance)' 'node'
-- Put the given node into the current vertex cover solution and
-- deletes all edges connected to that node into the given graph instance.
putInSolution :: (G.G, VC) -> G.Node -> (G.G, VC)
putInSolution (g, vc) v = (select g v, VC vertices) -- return a tuple
  where
    vertices = v : getVertices vc

-- |'putOthersInSolution' '(graph, vc instance)' 'node'
-- Put all nodes connected to the given node the current vertex cover solution
-- and deletes all edges connected to these nodes into the given graph instance.
putOthersInSolution :: (G.G, VC) -> G.Node -> (G.G, VC)
putOthersInSolution (g, vc) node =
  (go g elts, VC elts) -- return a tuple
  where
    edges = G.getEdges g
    elts = filterAdjacentNode edges node $ getVertices vc
    go g (e:es) = go (select g e) es
    go g [] = g

-- putOthersInSolution :: (G.G, VC) -> G.Node -> (G.G, VC)
-- putOthersInSolution (g, vc) node =
--   (go g elts, VC (elts ++ getVertices vc)) -- return a tuple
--   where
--     edges = G.getEdges g
--     elts = acc map fst (filter (\(x, _) -> x == node) edges) $
--       map . snd $ filter (\(x, _) -> x == node) edges
--       where
--         acc (x:xs) elts = acc xs x:elts
--         acc [] elts = elts
--     go g (e:es) = go (select g e) es
--     go g [] = g

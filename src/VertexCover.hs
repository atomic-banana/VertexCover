module VertexCover (
    VC(..)
    , emptyVertexCover
    , select
    , writeVCTo
    , putInSolution
    , putOthersInSolution
    , findAndSelectAllOneDegree
) where

import qualified Data.List  as L
import qualified Graph      as G
import qualified System.IO          as IO
import qualified Control.Exception as E

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

-- |'findOneDegreeVertex' 'graph'
-- Search each vertex which is one-degree.
findOneDegreeVertex :: G.G -> [G.Node]
findOneDegreeVertex g =
  -- Create tuples that associates the index with the number of edge only
  -- for vertices with one degree.
  [node | node <- [1..G.getNVertices g], G.howManyEdgeFor g node == 1]

-- |'findAndSelectAllOneDegree' 'instance'
-- Find and select all one-degree vertex from the graph and add neighbor into the VertexCover.
findAndSelectAllOneDegree :: (G.G, VC) -> (G.G, VC)
findAndSelectAllOneDegree (g, vc) =
  selectAllOneDegree (findOneDegreeVertex g) (g, vc)

-- |'selectAllOneDegree' '[nodes]' 'instance'
-- Select all one-degree vertex and add their neighbor into the solution
selectAllOneDegree :: [G.Node] -> (G.G, VC) -> (G.G, VC)
selectAllOneDegree (x:xs) result = selectAllOneDegree xs $ putOthersInSolution result x
selectAllOneDegree [] result = result

-- |'select' 'v' select a vertices to put to the solution.
-- return a new graph without the selected vertices.
select :: G.G -> G.Node -> Either String G.G
select (G.G 0 _ _)  v = Left "Cannot use an empty graph"
select g            v = Right G.G {
    G.getNVertices = G.getNVertices g - 1
  , G.getNEdges    = length edges
  , G.getEdges     = edges
  }
  where
    edges = filter (\x -> fst x /= v && snd x /= v) $ G.getEdges g

-- |'putInSolution' '(graph, vc instance)' 'node'
-- Put the given node into the current vertex cover solution and
-- deletes all edges connected to that node into the given graph instance.
putInSolution :: (G.G, VC) -> G.Node -> (G.G, VC)
putInSolution (g, vc) node = case select g node of
  Left msg -> error msg
  Right g -> (g, VC vertices) -- return a tuple
  where
    vertices = node : getVertices vc


-- |'putOthersInSolution' '(graph, vc instance)' 'node'
-- Put all nodes connected to the given node the current vertex cover solution
-- and deletes all edges connected to these nodes into the given graph instance.
putOthersInSolution :: (G.G, VC) -> G.Node -> (G.G, VC)
putOthersInSolution (g, vc) node =
  (go g elts (length elts - length (getVertices vc)), VC elts) -- return a tuple
  -- (go g elts (length elts - length (getVertices vc)), VC elts) -- return a tuple
  where
    edges = G.getEdges g
    elts = G.filterAdjacentNode edges node $ getVertices vc
    go g _ 0 =  g
    go g [] _ = g
    go g (e:es) cpt = case select g e of
      Left msg -> error msg
      Right g -> go g es $ cpt - 1
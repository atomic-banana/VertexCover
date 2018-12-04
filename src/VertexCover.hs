module VertexCover (
    VC(..)
    , select
) where

import qualified Graph as G

-- |A vertex cover solution
data VC = VC { getVertices :: [Int] } deriving (Show)

-- |'select' 'v' select a vertices to put to the solution.
-- return a new graph without the selected vertices.
select :: G.G -> Int -> G.G
select (G.G 0 _ _)  v = error "Cannot use an empty graph"
select g            v = G.G (G.getNVertices g - 1) (length edges) edges
    where 
        edges = filter (\x -> fst x /= v && snd x /= v) (G.getEdges g)

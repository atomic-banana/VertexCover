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
select :: G.G -> Int -> G.G
select (G.G 0 _ _)  v = error "Cannot use an empty graph"
select g            v = G.G (G.getNVertices g - 1) (length edges) edges
  where
    edges = filter (\x -> fst x /= v && snd x /= v) (G.getEdges g)

putInSolution :: (G.G, VC) -> Int -> (G.G, VC)
putInSolution x v = (select (fst x) v, VC vertices) -- return a tuple
  where
    vertices = v : getVertices (snd x)

putOthersInSolution :: (G.G, VC) -> Int -> (G.G, VC)
putOthersInSolution x v =
  (go g elts, VC (elts ++ getVertices (snd x))) -- return a tuple
  where
    g = fst x
    edges = G.getEdges g
    elts = map fst (filter (\x -> snd x == v) edges) ++
      map snd (filter (\x -> fst x == v) edges)
    go g (e:es) = go (select g e) es
    go g [] = g

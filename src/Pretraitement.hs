module Pretraitement (
  findFirstDegreeVertex
) where
import qualified Graph as G

findFirstDegreeVertex :: G.G -> [Int]
findFirstDegreeVertex g = [howManyEdgeFor x | x <- [1..G.getNVertices g]]
  where
    howManyEdgeFor n = sum [1 | (x, y) <- G.getEdges g, isEqualToOneOfThem x y, isNotTheOnlyVertex x y]
      where
        isEqualToOneOfThem x y = x == n || y == n
        isNotTheOnlyVertex x y = x /= n || y /= n




module Pretraitement (
  findFirstDegreeVertex,
  findLoopedFirstDegreeVertex
) where
import qualified Graph as G

findFirstDegreeVertex :: G.G -> [Int]
findFirstDegreeVertex g = [i | (i,x)<- zip [1..] countAllN, x == 1]
  where
    countAllN = [howManyEdgeFor x | x <- [1..G.getNVertices g]]
    howManyEdgeFor n = sum [1 | (x, y) <- G.getEdges g, isEqualToOneOfThem x y, isNotTheOnlyVertex x y]
      where
        isEqualToOneOfThem x y = x == n || y == n
        isNotTheOnlyVertex x y = x /= n || y /= n


findLoopedFirstDegreeVertex :: G.G -> [Int]
findLoopedFirstDegreeVertex g = [i | i <- [1.. G.getNVertices g], isLooped i]
  where
    isLooped n = any (==True) [isEqualToBothOfThem n x y |  (x, y) <- G.getEdges g]
    isEqualToBothOfThem n x y = x == n && y == n




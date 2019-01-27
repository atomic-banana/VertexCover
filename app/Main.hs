module Main where

import qualified Data.List          as L
import qualified System.Environment as Environment
import qualified System.IO          as IO
import qualified Graph              as G
import qualified VertexCover        as V
import qualified Data.Foldable      as F
import qualified Data.Function      as Fun


main :: IO ()
main = do
  args    <- Environment.getArgs
  content <- IO.readFile $ L.head args -- first argument -> input path file
  let maybeG = G.parseG $ L.lines content
  let output = args !! 1 -- second argument -> output path file
  IO.putStr "\nComputing...\n"
  IO.putStr "\nPrintG\n"
  IO.putStr $ show maybeG
  tryToComputeVertexCover maybeG output


tryToComputeVertexCover :: Maybe G.G -> String -> IO()
tryToComputeVertexCover (Just g) output =
  V.writeVCTo g vc output
  where
    result = getMinimumsVertexCovers . doFullTree $ V.findAndSelectAllOneDegree (g, V.emptyVertexCover)
    vc = snd result
tryToComputeVertexCover _ _ = error "File format not correct, cannot parse the graph"


getMinimumsVertexCovers :: [(G.G, V.VC)] -> (G.G, V.VC)
getMinimumsVertexCovers leafs = head $ L.sortBy (compare `Fun.on` (length . V.getVertices . snd)) leafs


doFullTree :: (G.G, V.VC) -> [(G.G, V.VC)]
doFullTree (g,vc)
  | G.getEdges g == [] = (g,vc):[]
  | otherwise = (doFullBranch V.putInSolution) ++ (doFullBranch V.putOthersInSolution)
  where
    vtx = F.minimum (L.map (uncurry min) $ G.getEdges g)
    doFullBranch putSomeInSolution = doFullTree $ doOneBranch vtx putSomeInSolution (g,vc)


doOneBranch :: G.Node -> ((G.G, V.VC) -> G.Node -> (G.G, V.VC)) -> (G.G, V.VC) -> (G.G, V.VC)
doOneBranch toTreat putSomeInSolution x = putSomeInSolution x toTreat
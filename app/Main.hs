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
  -- where
  --   continue (Just g) = V.writeVCTo g vc output
  --   result = computeVertexCover maybeG
  --   g = fst result
  --   vc = snd result
  --   computeVertexCover :: Maybe G.G -> (G.G, V.VC)
  --   computeVertexCover (Just g) = V.findAndSelectAllOneDegree (g, V.emptyVertexCover)

tryToComputeVertexCover :: Maybe G.G -> String -> IO()
tryToComputeVertexCover (Just g) output =
  V.writeVCTo g vc output
  where
    result = getMinimumsVertexCovers . doFullTree $ V.findAndSelectAllOneDegree (g, V.emptyVertexCover)
    vc = snd result
tryToComputeVertexCover _ _ = error "File format not correct, cannot parse the graph"

    -- ( doFullTree (V.selectAllOneDegree (g, V.emptyVertexCover)) )


getMinimumsVertexCovers :: [(G.G, V.VC)] -> (G.G, V.VC)
getMinimumsVertexCovers leafs = head $ L.sortBy (compare `Fun.on` (length . V.getVertices . snd)) leafs

--   [x | x <- zip [length leaf | leaf <- leafs] leafs]



doFullTree :: (G.G, V.VC) -> [(G.G, V.VC)]
doFullTree (g,vc)
  | G.getEdges g == [] = (g,vc):[]
  | otherwise = (doFullBranch V.putInSolution) ++ (doFullBranch V.putOthersInSolution)
  where
    vtx = F.minimum (L.map (uncurry min) $ G.getEdges g)
    doFullBranch putSomeInSolution = doFullTree $ doOneBranch vtx putSomeInSolution (g,vc)


doOneBranch :: G.Node -> ((G.G, V.VC) -> G.Node -> (G.G, V.VC)) -> (G.G, V.VC) -> (G.G, V.VC)
doOneBranch toTreat putSomeInSolution x = putSomeInSolution x toTreat





-- Old main
-- main :: IO ()
-- main = do
--   args    <- Environment.getArgs
--   content <- IO.readFile (L.head args)
--   let g = G.parseG $ L.lines content
--   IO.putStr "\nPrintG\n"
--   IO.putStr $ show g
--   IO.putStr "\n\nprintVertexesToDelete\n"
--   printVertexesToDelete g
--   IO.putStr "\n"
--   V.writeVCTo g ( doFullTree doPreTreatFull (g, V.VC{V.getVertices = []}) ) "test.vc"
--   printResult g
--   where
--     printVertexesToDelete (Just g)
--       | G.checkG g = print $ P.findOneDegreeVertex g
--       | otherwise  = printResultError
--     printVertexesToDelete _ = printResultError

{-
printResult :: (G.G -> [Int]) -> ((G.G, V.VC) -> Int -> (G.G, V.VC)) -> Maybe G.G -> IO ()
printResult findVertex putSomeInSolution (Just g) = treat
  where
    treat
      | G.checkG g = print $ putSomeInSolution (g, V.VC{V.getVertices = []}) $ head vertexToDelete
      | otherwise  = printResultError
      where
        vertexToDelete = findVertex g
printResult _ _ _                                 = printResultError
-}

-- printResultError :: IO ()
-- printResultError = print $ "ERROR, graph isn't valid"
--
--
--
--
-- printResult :: Maybe G.G -> IO ()
-- printResult (Just g)
--   | G.checkG g = print $ doTree
--   | otherwise  = printResultError
--     where
--       doTree = doFullTree doPreTreat
--       doPreTreat = doPreTreatFull(g, V.VC{V.getVertices = []})
-- printResult _ = printResultError

{-
doFullTree :: (G.G, V.VC) -> Int--[(G.G, V.VC)]
doFullTree (g,vc)
  | G.getEdges g == [] = 0
  | otherwise = 1
-}


--doOneBranchPreTreat (findVertex g) putSomeInSolution


-- doPreTreatFull x = doPreTreatFDV x
-- doPreTreatFDV x = doOneBranchPreTreat (P.findOneDegreeVertex $ fst x) V.putOthersInSolution x

-- doOneBranchPreTreat :: [Int] -> ((G.G, V.VC) -> Int -> (G.G, V.VC)) -> (G.G, V.VC) -> (G.G, V.VC)
-- doOneBranchPreTreat (toDelete:vtxsLeft) putSomeInSolution x = doOneBranchPreTreat vtxsLeft putSomeInSolution $ putSomeInSolution x toDelete
-- doOneBranchPreTreat (toDelete:[]) putSomeInSolution x = doOneBranchPreTreat [] putSomeInSolution $ putSomeInSolution x toDelete
-- doOneBranchPreTreat [] _ x = x




{-
  printresult
  where
    graph = G.G { G.getNVertices = 7, G.getNEdges = 7, G.getEdges = [(6,2),(1,2),(3,2),(7,3),(1,5),(1,4),(4,3)] }
    printresult
      | G.checkG graph = print $ P.findFirstDegreeVertex graph
      | otherwise    = print $ "ERROR, graph isn't valid"

  -- do
  -- args    <- Environment.getArgs
  -- content <- IO.readFile (L.head args)
  -- IO.putStr . show . G.parseG $ L.lines content
  -- IO.putStr "\n"
 -}

{-


    tryToSave (G.parseG (L.lines content)) vc "test.tmp"
  where
    vc = V.VC [1, 2, 3]

tryToShow :: Maybe G.G -> String
tryToShow (Just g) = "Correct " ++ show (V.select g 4)
tryToShow _        = "Error, no graph parsed"

tryToSave :: Maybe G.G -> V.VC -> String -> IO()
tryToSave (Just g) vc path = V.writeVCTo g vc path
tryToSave _ _ _       = error "Error, no graph parsed"

-}


{-
  print $ P.findLoopedFirstDegreeVertex G.G { G.getNVertices = 7, G.getNEdges = 7, G.getEdges = [(6,6),(1,2),(3,2),(7,3),(1,5),(1,4),(4,3)] }
  result awaited : [6]
-}
{-
  print $ P.findFirstDegreeVertex G.G { G.getNVertices = 7, G.getNEdges = 7, G.getEdges = [(6,2),(1,2),(3,2),(7,3),(1,5),(1,4),(4,3)] }
  result awaited : [5,6,7]
-}

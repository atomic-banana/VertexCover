module Main where

import qualified Data.List          as L
import qualified System.Environment as Environment
import qualified System.IO          as IO
import qualified Graph              as G
import qualified VertexCover        as V
import qualified Pretraitement      as P 


main :: IO ()
main = do
  args    <- Environment.getArgs
  content <- IO.readFile (L.head args)
  let g = G.parseG $ L.lines content 
  IO.putStr "\n" 
  IO.putStr $ show g
  IO.putStr "\n" 
  printVertexesToDelete g
  IO.putStr "\n" 
  printResult P.findFirstDegreeVertex V.putOthersInSolution g  
  where 
    printVertexesToDelete (Just g)
      | G.checkG g = print $ P.findFirstDegreeVertex g
      | otherwise  = printResultError
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

printResultError :: IO ()
printResultError = print $ "ERROR, graph isn't valid"




printResult :: (G.G -> [Int]) -> ((G.G, V.VC) -> Int -> (G.G, V.VC)) -> Maybe G.G -> IO ()
printResult findVertex putSomeInSolution (Just g) = treat
  where 
    treat
      | G.checkG g = print $ doOneBranch vertexToDelete putSomeInSolution (g, V.VC{V.getVertices = []}) 
      | otherwise  = printResultError
      where 
        vertexToDelete = findVertex g
printResult _ _ _                                 = printResultError




doOneBranch :: [Int] -> ((G.G, V.VC) -> Int -> (G.G, V.VC)) -> (G.G, V.VC) -> (G.G, V.VC)
doOneBranch (toDelete:vtxsLeft) putSomeInSolution x = doOneBranch vtxsLeft putSomeInSolution (putSomeInSolution x toDelete) 
doOneBranch [] _ x = x





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
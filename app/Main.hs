module Main where

import qualified Data.List          as L
import qualified System.Environment as Environment
import qualified System.IO          as IO
import qualified Graph              as G
import qualified VertexCover        as V

main :: IO ()
main = do
    args    <- Environment.getArgs
    content <- IO.readFile (L.head args)
    tryToSave (G.parseG (L.lines content)) vc "test.tmp"
  where
    vc = V.VC [1, 2, 3]

tryToShow :: Maybe G.G -> String
tryToShow (Just g) = "Correct " ++ show (V.select g 4)
tryToShow _        = "Error, no graph parsed"

tryToSave :: Maybe G.G -> V.VC -> String -> IO()
tryToSave (Just g) vc path = V.writeVCTo g vc path
tryToSave _ _ _       = error "Error, no graph parsed"

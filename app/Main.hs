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
    IO.putStr . tryTo . G.parseG $ L.lines content
    IO.putStr "\n"

tryTo :: Maybe G.G -> String
tryTo (Just g)      = "Correct " ++ (show $ V.select g 4)
tryTo (Nothing)     = "Error, no graph parsed"


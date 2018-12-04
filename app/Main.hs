module Main where

-- import qualified Data.Foldable      as F
import qualified Data.List          as L
-- import qualified Data.Tuple         as T
import qualified System.Environment as Environment
import qualified System.IO as IO
import qualified Graph as G

-- Parse and display a graph.
main :: IO ()
main = do
  args    <- Environment.getArgs
  content <- IO.readFile (L.head args)
  IO.putStr . show . G.parseG $ L.lines content
  IO.putStr "\n"
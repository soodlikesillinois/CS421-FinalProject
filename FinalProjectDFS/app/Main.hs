import Data.Array
import Lib as DFSOld
import LibNew as DFSNew
import System.Random
import Control.DeepSeq
import Data.Time.Clock 

main :: IO ()
--     1
--    / \
--   2   3
--    \ / \
--     4   5
--      \ /
--       6
exampleGraph :: Graph
exampleGraph = array (1, 6) 
    [ (1, [2, 3])
    , (2, [4])
    , (3, [4, 5])
    , (4, [6])
    , (5, [6])
    , (6, [])
    ]

-- Generate a large random DAG
generateLargeDAG :: Int -> IO Graph
generateLargeDAG n = do
  gen <- getStdGen
  let edges = [(i, j) | i <- [1..n], j <- [1..n], i < j, fst (random gen) < (0.1 :: Double)]
  return $ array (1, n) [(i, [j | (x, j) <- edges, x == i]) | i <- [1..n]]

createBounds :: Int -> Int -> Bounds
createBounds start size = (start, size)

main = do
  putStrLn $ "Running paper's DFS Numbering with example graph..."
  let oldresult = DFSOld.preArr (createBounds 1 6) (dff exampleGraph)
  print oldresult

  putStrLn $ "Running a modern DFS Numbering with example graph..."
  let newresult = DFSNew.preArr exampleGraph
  print newresult
  
  let size = 10000  -- Size of the graph
  putStrLn $ "Generating a random DAG with " ++ show size ++ " vertices..."
  graph <- generateLargeDAG size
  
  putStrLn "Running DFS Numbering from the paper..."
  start <- getCurrentTime
  let oldresult' = DFSOld.preArr (createBounds 1 size) (dff graph)
  oldresult' `deepseq` return ()  
  end <- getCurrentTime
  
  let diff = diffUTCTime end start
  putStrLn $ "The paper's DFS Numbering completed in " ++ show diff

  putStrLn "Running modern DFS Numbering..."
  start <- getCurrentTime
  let newresult' = DFSNew.preArr graph
  newresult' `deepseq` return ()  
  end <- getCurrentTime
  
  let diff = diffUTCTime end start
  putStrLn $ "The modern DFS Numbering completed in " ++ show diff


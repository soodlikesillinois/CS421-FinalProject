import Data.Graph
import Data.Array
import System.Random
import Control.Monad
import Data.Time.Clock
import qualified Data.Set as Set


generateRandomDAG :: Int -> IO [(Int, Int, [Int])]
generateRandomDAG n = forM [1..n] $ \v -> do
    numEdges <- randomRIO (0, min 5 (n - v))
    targets <- replicateM numEdges (randomRIO (v + 1, n))
    return (v, v, Set.toList $ Set.fromList targets)  


--     1
--    / \
--   2   3
--    \ / \
--     4   5
--      \ /
--       6

simpleDAG :: [(Int, Int, [Int])]
simpleDAG = [
    (1, 1, [2, 3]),
    (2, 2, [4]),
    (3, 3, [4, 5]),
    (4, 4, [6]),
    (5, 5, [6]),
    (6, 6, [])
    ]

main :: IO ()
main = do
    let n = 10000  
    putStrLn $ "Generating random DAG with " ++ show n ++ " vertices..."
    
    start <- getCurrentTime
    dagEdges <- generateRandomDAG n
    -- let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges simpleDAG
    let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges dagEdges

    
    putStrLn "Running topological sort from Data.Graph..."
    let sortedVertices = topSort graph

    -- putStrLn "\nSorted nodes:"
    -- let sortedNodes = map ((\(node,_,_) -> node) . nodeFromVertex) sortedVertices
    -- print sortedNodes
    
    end <- getCurrentTime
    
    let duration = diffUTCTime end start
    putStrLn $ "DAG generation and topological sort completed in " ++ show duration

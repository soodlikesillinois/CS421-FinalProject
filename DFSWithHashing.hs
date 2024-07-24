{-
Using hashing to implement DFS in Haskell using the Data.HashSet and Data.HashMap.Strict from the unordered-containers package.
-}

import Data.Graph
import Data.Tree
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.State

type Vertex = Int
type Graph = HashMap Vertex [Vertex]

-- State for DFS including visited nodes and other metadata
type DFSState = (HashSet Vertex, HashMap Vertex Int, HashMap Vertex Int, Int)

-- Depth-First Search with Hashing
dfs :: Graph -> Vertex -> State DFSState (Tree Vertex)
dfs graph v = do
    (visited, pre, low, time) <- get
    let visited' = HashSet.insert v visited
    let pre' = HashMap.insert v time pre
    let time' = time + 1
    put (visited', pre', low, time')
    children <- mapM (dfsVisit graph v) (HashMap.lookupDefault [] v graph)
    (_, _, low', _) <- get
    let lv = minimum (pre' HashMap.! v : [low' HashMap.! w | Node w _ <- children])
    let low'' = HashMap.insert v lv low'
    put (visited', pre', low'', time')
    return $ Node v children

-- Visit a node if not already visited
dfsVisit :: Graph -> Vertex -> Vertex -> State DFSState (Tree Vertex)
dfsVisit graph parent v = do
    (visited, _, _, _) <- get
    if HashSet.member v visited
        then return $ Node v []
        else dfs graph v

-- Main function to run DFS on a graph
runDFS :: Graph -> Vertex -> Tree Vertex
runDFS graph start = evalState (dfs graph start) (HashSet.empty, HashMap.empty, HashMap.empty, 0)

-- Example usage
main :: IO ()
main = do
    let graph = HashMap.fromList [(0, [1, 2]), (1, [0, 3]), (2, [0]), (3, [1, 4, 5]), (4, [3]), (5, [3, 6]), (6, [5, 7, 8]), (7, [6]), (8, [6])]
    print $ runDFS graph 0
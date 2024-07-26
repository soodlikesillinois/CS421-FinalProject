module LibNew where 
import Data.Graph
import Data.Array

-- Perform DFS and return the vertices in preorder
preorder :: Tree a -> [a]
preorder (Node a ts) = a : concatMap preorder ts

-- Create an array that maps each vertex to its DFS number
preArr :: Graph -> Array Vertex Int
preArr g = tabulate bounds (preorderF (dfs g (vertices g))) 
  where
    bounds = getBounds g
    forest = dfs g (vertices g)

-- Tabulate the preorder sequence to an array
tabulate :: (Vertex, Vertex) -> [Vertex] -> Array Vertex Int
tabulate bnds vs = array bnds (zip vs [1..])

-- Convert the DFS forest to a preorder list
preorderF :: Forest Vertex -> [Vertex]
preorderF = concatMap preorder

-- Get bounds of a graph
getBounds :: Graph -> (Vertex, Vertex)
getBounds g = (minimum vs, maximum vs)
  where
    vs = vertices g
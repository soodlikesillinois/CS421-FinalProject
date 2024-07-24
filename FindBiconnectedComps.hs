import Data.Graph (Vertex, dfs)
import Data.Tree
import Data.Array

type Table a = Array Vertex a 
type Graph = Table [Vertex]

vertices :: Graph -> [Vertex]
vertices = indices

--extract a list of edges from the graph, this is done with the function edges.
--An edge is a pair of vertices
type Edge = (Vertex,Vertex)

edges :: Graph -> [Edge]
edges g = [(v,w) | v <- vertices g, w <- g!v]

--To manipulate tables (including graphs) we provide a generic function 
--mapT which applies its function argument to every table index fentry pair,
--and builds a new table.
mapT :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [(v, f v (t!v)) | v<-indices t]

type Bounds = (Vertex, Vertex)

--builds a table detailing the number of edges leaving each vertex.
findoutdegree :: Graph -> Table Int
findoutdegree g = mapT numEdges g
    where numEdges v ws = length ws

--build up a graph from a list of edges we define buildG
{-Like array the Haskell function accumArray builds an array from
 a list of index/value pairs, with the difference that
accumArray accepts possibly many values for each indexed
location, which are combined using the function provided as
accunulrray’s first argument.-}
buildGraph :: Bounds -> [Edge] -> Graph
buildGraph bnds es = accumArray (flip (:)) [] bnds es

--Combining the functions edges and buildGraph gives us a
--way to reverse all the edges in a graph giving the transpose
--of the graph:
transpose :: Graph -> Graph
transpose g = buildGraph (bounds g) (reverseE g)

reverseE :: Graph -> [Edge]
reverseE g = [ (w,v) | (v,w) <- edges g]

{- Rather than defining indegree from scratch
by, for example, building an array incrementally as we traverse the graph,
we simply reuse previously defined functions, combining them in afresh way.
The result is shorter and clearer-}
indegree :: Graph -> Table Int
indegree g = findoutdegree (transpose g)

{- BiConnected Components
We end by programming a more complex algorithm—finding
biconnected components. An undirected graph is biconnected
if the removal of any vertex leaves the remaining subgraph
connected. This has a bearing in the problem of reliability in
communication networks. For example, if you want to avoid
driving through a particular town, is there an alternative
route?
-}

-- Calculate the depth-first spanning forest
dfforest :: Graph -> Forest Vertex
dfforest g = dfs g [0 .. n] where (_, n) = bounds g

-- Calculate pre-order numbering
preArr :: Bounds -> Forest Vertex -> Table Int
preArr bnds forest = array bnds (zip (concatMap flatten forest) [0..])

-- Label the tree with depth-first numbers and low-point numbers
label :: Graph -> Table Int -> Tree Vertex -> Tree (Vertex, Int, Int)
label g dnumber (Node v ts) = Node (v, dnumber ! v, lv) us
  where
    us = map (label g dnumber) ts
    lv = minimum ([dnumber ! v] ++ [dnumber ! w | w <- g ! v] ++ [lu | Node (u, du, lu) _ <- us])

-- Collect biconnected components from the labeled tree
collect :: Tree (Vertex, Int, Int) -> (Int, Tree [Vertex])
collect (Node (v, dv, lv) ts) = (lv, Node (v : vs) cs)
  where
    collected = map collect ts
    vs = concat [ws | (lw, Node ws _) <- collected, lw < dv]
    cs = concat [if lw < dv then us else [Node (v : ws) us] | (lw, Node ws us) <- collected]

-- Handle the special case of the root
bicomponents :: Tree (Vertex, Int, Int) -> Forest [Vertex]
bicomponents t@(Node (v, dv, lv) ts) = [Node (v : vs) us | (lw, Node vs us) <- collected]
  where
    collected = map collect ts

-- Main function to find biconnected components
bconnectedcomps :: Graph -> Forest [Vertex]
bconnectedcomps g = concatMap (bicomponents . label g dnumber) forest
  where
    forest = dfforest g
    dnumber = preArr (bounds g) forest

-- Example usage
main :: IO ()
main = do
  let g = buildGraph (0, 8) [(0, 1), (1, 2), (2, 0), (1, 3), (3, 4), (4, 5), (5, 3), (5, 6), (6, 7), (7, 8), (8, 6)]
  print (bconnectedcomps g)

module Lib where
import Data.Array
import Control.Monad.ST
import Data.Array.ST
import System.Random
import Control.DeepSeq

type Table a = Array Vertex a
type Graph = Table [Vertex]
type Vertex = Int
type Edge = (Vertex ,Vertex)
type Bounds = (Vertex, Vertex)
data Tree a = Node a (Forest a) 
type Forest a = [Tree a]
type Set s = STArray s Vertex Bool

vertices :: Graph -> [Vertex]
vertices = indices

edges :: Graph -> [Edge]
edges g = [ (v, w) | v <- vertices g, w <- g!v]

mapT :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t)
  [(v, f v (t!v)) | v <- indices t]

outdegree :: Graph -> Table Int
outdegree g = mapT numEdges g
  where numEdges v ws = length ws

buildG :: Bounds -> [Edge] -> Graph
buildG bnds es = accumArray (flip (:)) [] bnds es

transposeG :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE :: Graph -> [Edge]
reverseE g = [ (w,v) | (v,w) <- edges g]

indegree :: Graph -> Table Int
indegree g = outdegree (transposeG g)

dfs :: Graph -> [Vertex] -> Forest Vertex
dfs g vs = prune (bounds g) (map (generate g) vs)

dff :: Graph -> Forest Vertex
dff g = dfs g (vertices g)

preorder :: Tree a -> [a]
preorder (Node a ts) = [a] ++ preorderF ts

preorderF :: Forest a -> [a]
preorderF ts = concat (map preorder ts)

preOrd :: Graph -> [Vertex]
preOrd g = preorderF (dff g)

tabulate :: Bounds -> [Vertex] -> Table Int
tabulate bnds vs = array bnds (zip vs [1..])

preArr :: Bounds -> Forest Vertex -> Table Int
preArr bnds ts = tabulate bnds (preorderF ts)

generate :: Graph -> Vertex -> Tree Vertex
generate g v = Node v (map (generate g) (g ! v))

mkEmpty :: Bounds -> ST s (Set s)
mkEmpty bnds = newArray bnds False

prune :: Bounds -> Forest Vertex -> Forest Vertex
prune bnds ts = runST $ do
  m <- mkEmpty bnds
  chop m ts

chop :: Set s -> Forest Vertex -> ST s (Forest Vertex)
chop m [] = return []
chop m (Node v ts : us) = do
  visited <- readArray m v
  if visited
    then chop m us
    else do
      writeArray m v True
      as <- chop m ts
      bs <- chop m us
      return (Node v as : bs)









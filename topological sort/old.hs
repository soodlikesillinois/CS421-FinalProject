import Data.Array
import Control.Monad.ST
import Data.Array.ST
import Data.Time
import System.Random
import Control.DeepSeq

type Vertex = Int
type Graph = Array Vertex [Vertex]
type Bounds = (Vertex, Vertex)
type Forest a = [Tree a]
data Tree a = Node a (Forest a) deriving Show

type Set s = STArray s Vertex Bool

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

prune :: Bounds -> Forest Vertex -> Forest Vertex
prune bnds ts = runST $ do
  m <- mkEmpty bnds
  chop m ts

mkEmpty :: Bounds -> ST s (Set s)
mkEmpty bnds = newArray bnds False

topSort :: Graph -> [Vertex]
topSort g = reverse (postOrd g)

postOrd :: Graph -> [Vertex]
postOrd g = postorderF (dff g)

postorder :: Tree a -> [a]
postorder (Node a ts) = postorderF ts ++ [a]

postorderF :: Forest a -> [a]
postorderF ts = concat (map postorder ts)

dff :: Graph -> Forest Vertex
dff g = dfs g (vertices g)

dfs :: Graph -> [Vertex] -> Forest Vertex
dfs g vs = prune (bounds g) (map (generate g) vs)

vertices :: Graph -> [Vertex]
vertices = indices

generate :: Graph -> Vertex -> Tree Vertex
generate g v = Node v (map (generate g) (g ! v))

-- Generate a large random DAG
generateLargeDAG :: Int -> IO Graph
generateLargeDAG n = do
  gen <- getStdGen
  let edges = [(i, j) | i <- [1..n], j <- [1..n], i < j, fst (random gen) < (0.1 :: Double)]
  return $ array (1, n) [(i, [j | (x, j) <- edges, x == i]) | i <- [1..n]]


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


main :: IO ()
main = do
  let size = 10000  -- Size of the graph
  putStrLn $ "Generating a random DAG with " ++ show size ++ " vertices..."
  graph <- generateLargeDAG size
  
  putStrLn "Running topological sort from the paper..."
  start <- getCurrentTime
  let result = topSort graph
  result `deepseq` return ()  
  end <- getCurrentTime
  
  let diff = diffUTCTime end start
  putStrLn $ "Topological sort completed in " ++ show diff
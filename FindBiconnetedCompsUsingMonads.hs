import Control.Monad.State
import Data.Array
import Data.Graph
import Data.Tree
import Data.List

type Table a = Array Vertex a
type BCCState = (Table Int, Table Int, Int)

-- Calculate the depth-first spanning forest
dff :: Graph -> Forest Vertex
dff g = dfs g (vertices g)

-- Calculate pre-order numbering
preArr :: Bounds -> Forest Vertex -> Table Int
preArr bnds forest = array bnds (zip (concatMap flatten forest) [0..])

-- Label the tree with depth-first numbers and low-point numbers
label :: Graph -> Tree Vertex -> State BCCState (Tree (Vertex, Int, Int))
label g (Node v ts) = do
    (pre, low, time) <- get
    let pre' = pre // [(v, time)]
    let time' = time + 1
    put (pre', low, time')
    us <- mapM (label g) ts
    (_, low', _) <- get
    let lv = minimum ([pre' ! v] ++ [pre' ! w | w <- g ! v] ++ [lu | Node (_, _, lu) _ <- us])
    let low'' = low' // [(v, lv)]
    put (pre', low'', time')
    return (Node (v, pre' ! v, lv) us)

-- Collect biconnected components from the labeled tree
collect :: Tree (Vertex, Int, Int) -> (Int, Tree [Vertex])
collect (Node (v, dv, lv) ts) = (lv, Node (v : vs) cs)
  where
    collected = map collect ts
    vs = concat [ws | (lw, Node ws _) <- collected, lw < dv]
    cs = concat [if lw < dv then us else [Node (v : ws) us] | (lw, Node ws us) <- collected]

-- Handle the special case of the root
bicomps :: Tree (Vertex, Int, Int) -> Forest [Vertex]
bicomps t@(Node (v, dv, lv) ts) = [Node (v : vs) us | (lw, Node vs us) <- collected]
  where
    collected = map collect ts

-- Main function to find biconnected components
bcc :: Graph -> Forest [Vertex]
bcc g = evalState (mapM (label g) forest >>= return . concatMap bicomps) initialState
  where
    forest = dff g
    bnds = bounds g
    initialState = (array bnds [(i, -1) | i <- range bnds], array bnds [(i, -1) | i <- range bnds], 0)

-- Example usage
main :: IO ()
main = do
  let g = buildG (0, 8) [(0, 1), (1, 2), (2, 0), (1, 3), (3, 4), (4, 5), (5, 3), (5, 6), (6, 7), (7, 8), (8, 6)]
  print (bcc g)
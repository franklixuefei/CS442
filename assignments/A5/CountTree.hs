data State s a = ST (s-> (a, s))
data Tree a = Empty | Node (a, Tree a, Tree a)
instance Monad (State s) where
 return x = ST (\s -> (x, s))
 (>>=) (ST f) g = ST (\s0 ->
  let 
   (a, s1) = f s0
   (ST h) = g a
   (b, s2) = h s1
  in
   (b, s2)
  )
 
inc:: State Int ()
inc = ST (\n -> ((), n + 1))

numNodes :: Tree a -> State Int ()
numNodes Empty = return ()
numNodes (Node (_, left, right)) = 
  inc >>= \_ ->
  numNodes left >>= \_ ->
  numNodes right
ct = 
 let 
  ST f = numNodes (Node (11, Node(23, Empty, Node (58, Empty, Empty)), Empty))
 in
  f 0
data State s a = ST(s -> (a, s))

instance Monad (State s) where
   return x = ST(\s -> (x, s))

   (ST x) >>= f = ST (\s0 -> let (a, s1) = x s0
                                 (ST g) = f a
                                 (b, s2) = g s1
                             in
                                 (b, s2)
                     )
data Tree a = Empty | Node (a, Tree a, Tree a) deriving Show

type MyState a = (a -> Maybe Int, Int)

updateMap :: Eq a => (a -> b) -> a -> b -> (a -> b)
updateMap f x y = \z -> if z == x then 
							y 
						else 
							f z
labelNode :: Eq t => t -> State (MyState t) Int
labelNode x =
   ST (
     \(f, n) -> if f x == Nothing then
                   (n, (updateMap f x (Just n), n + 1))
                else
                   let Just m = f x
                   in (m, (f, n))
   )

labelTree :: Eq t => Tree t -> State (MyState t) (Tree Int)
labelTree Empty = return Empty
labelTree (Node (x, t1, t2)) =
     labelNode x >>= \ lx ->
     labelTree t1 >>= \ lt1 ->
     labelTree t2 >>= \ lt2 ->
     return (Node (lx, lt1, lt2))

labelTheTree :: Eq t => Tree t -> Tree Int
labelTheTree t = t'
   where
       ST s = labelTree t
       (t', _) = s (\_ -> Nothing, 0)

t = Node(45, 
         Node (54,
               Node (67,
                     Node (54, Empty, Empty),
                     Empty),
               Node (45,
                     Node (67,
                           Node (45, Empty, Empty),
                           Empty),
                     Empty)),
         Empty)

newT = labelTheTree t
--- A5A1 ---

--__/\\\\\\\\\\\\\\\________/\\\________        
-- _\/\\\///////////________\/\\\________       
--  _\/\\\______________/\\\_\/\\\________      
--   _\/\\\\\\\\\\\_____\///__\/\\\________     
--    _\/\\\///////_______/\\\_\/\\\\\\\\\__    
--     _\/\\\_____________\/\\\_\/\\\////\\\_   
--      _\/\\\_____________\/\\\_\/\\\__\/\\\_  
--       _\/\\\_____________\/\\\_\/\\\\\\\\\__ 
--        _\///______________\///__\/////////___

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

fibHelper :: Int -> [Int] -> State (Int, Int) [Int]
fibHelper 0 l = return l
fibHelper n l = ST (\(x, y) -> (l++[x], (y, x+y))) >>= \l -> (fibHelper (n-1) l)


fib :: Int -> [Int]
fib n = 
	let
		ST f = fibHelper n []
		(a, s) = f (0, 1)
	in
		a

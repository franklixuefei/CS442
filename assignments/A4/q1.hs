lazymap f (x:xs) = f x : lazymap f xs

-- Testing:
ones = 1 : ones
res = lazymap (3 +) ones
main = print (head (tail (tail (lazymap (3 +) [1 ..]))))

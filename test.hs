safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)
 
doit a b =
  (a `safeDiv` b) >>= 
  \res -> return (show res) >>=
  \ress -> putStrLn(ress) >>
  return 10
  
  
extract (Just a) = a
extract (Nothing) = "err"
  

  
  

	


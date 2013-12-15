--- A5A1 --- 

--_____/\\\\\\\\\\\\________________________________________________________        
-- ___/\\\//////////_________________________________________________________       
--  __/\\\____________________________________________________________________      
--   _\/\\\____/\\\\\\\__/\\\____/\\\_____/\\\\\\\\___/\\\\\\\\\\__/\\\\\\\\\\_     
--    _\/\\\___\/////\\\_\/\\\___\/\\\___/\\\/////\\\_\/\\\//////__\/\\\//////__    
--     _\/\\\_______\/\\\_\/\\\___\/\\\__/\\\\\\\\\\\__\/\\\\\\\\\\_\/\\\\\\\\\\_   
--      _\/\\\_______\/\\\_\/\\\___\/\\\_\//\\///////___\////////\\\_\////////\\\_  
--       _\//\\\\\\\\\\\\/__\//\\\\\\\\\___\//\\\\\\\\\\__/\\\\\\\\\\__/\\\\\\\\\\_ 
--        __\////////////_____\/////////_____\//////////__\//////////__\//////////__


import System.IO 
import Data.List.Split

guess :: IO ()
guess = do 
			putStr "Enter guessing range: "
			range <- getLine
			let rangePair = splitOn " " range
			let lower = (read (head rangePair)::Int)
			let higher = (read (last rangePair)::Int)
			guessHelper lower higher
			

			
			
			
guessHelper :: Int -> Int -> IO ()
guessHelper lower higher = do 									
									let median = (floor (fromIntegral (higher + lower)/2))
									let medianStr = show(median)
									putStr ("Is it "++medianStr++"?: ")
									answer <- getLine
									if answer == "higher" then do
										if (higher - median) == 0
											then do putStrLn "Cheating!"
										else guessHelper (median+1) higher
									else if answer == "lower" then do
										if (median - lower) == 0
											then do putStrLn "Cheating!"
										else guessHelper lower (median-1)
									else if answer == "yes"
										then do putStrLn "Got it!"
									else do
										guessHelper lower higher
										
										


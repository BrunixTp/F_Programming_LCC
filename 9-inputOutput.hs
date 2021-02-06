module InputOutput where

import System.Random

--a if the name isn't intuitive enough, something is wrong with you 
bingo :: IO () 
bingo = do newList <- acumNums []
           getChar
           showResults newList
        
acumNums :: [Int] -> IO [Int]
acumNums l | length l == 90 = do return l 
           | otherwise = do v <- randomRIO(1,90)
                            let newList = if v `elem` l then l
                                          else v:l in acumNums newList

showResults :: [Int] -> IO ()
showResults [] = do putStrLn "End, it's done mate"
showResults (x:xs) = do print x
                        getChar
                        showResults xs

--b 

                        
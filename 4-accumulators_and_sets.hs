 module Ficha4RE where

import Data.Char



-- [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]
-- [6,12,18]

-- [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]
-- [6,12,18]

-- [(x,y) | x <- [0..20], y <- [0..20], x + y == 30]
-- [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]

-- [sum [y | y <- [1..x], odd y] | x <- [1..10]]    
-- [1,1,4,4,9,9,16,16,25,25]

-- a [1,2,4,8,16,32,64,128,256,512,1024]
a = [2^x | x <- [0..10]]

-- b [(1,5),(2,4),(3,3),(4,2),(5,1)]
b = [(x,6-x) | x <- [1..5]]

-- c [[1], [1,2], [1,2,3], [1,2,3,4], [1,2,3,4,5]]
c = [[x | x <- [1..y]] | y <- [1..5]]

-- d [[1], [1,1], [1,1,1],[1,1,1,1], [1,1,1,1,1]]

d = [[1 | x <- [1..y]] | y <- [1..5]]

-- e [1,2,6,24,120,720]

e = [fact x | x <- [1..6]]
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)
-- 3 D
digitAlpha :: String -> (String, String)
digitAlpha [] = ([], []) 
digitAlpha (x:xs) | isDigit x = (x:ds, as)
                  | isAlpha x = (ds, x:as)
                  | otherwise = (ds, as)
                  where (ds,as) = digitAlpha xs

-- 4 nzp 
nzp :: [Int] -> (Int, Int, Int)
nzp [] = (0,0,0)
nzp (x:xs) | x > 0  =  (a,b,c+1)
           | x == 0 = (a,b+1,c)
           | x < 0 = (a+1,b,c)
           where (a,b,c) = nzp xs


-- 5

-- 6
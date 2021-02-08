module Exam1819 where

-- 1 
-- a
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [a] = True
isSorted (x:y:t) = x <= y && isSorted (y:t) 
-- b
inits' :: [a] -> [[a]] 
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

-- 2



-- 3 
data LTree a = Tip a | Fork (LTree a) (LTree a)

--a 

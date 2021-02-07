module Test where

-- 1

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) [] _ = []
(\\) x [] = x
(\\) (x:xs) (h:t) = remove h ((\\) xs t)
 where remove :: Eq a => a -> [a] -> [a]
       remove _ [] = []
       remove x (h:t) = if x == h then t
                        else h:remove x t

-- 2
type MSet a = [(a,Int)]
-- a

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = [] 
removeMSet n ((x,y):t) | n == x && y /= 1 = (x,y-1):t
                       | n == x && y == 1 = t 
                       | otherwise = (x,y):removeMSet n t

-- b
calculate :: MSet a -> ([a], Int)
calculate = foldl (\(acc1,acc2) (a,b)-> (a:acc1, b+acc2)) ([],2)

-- 3
{- partes :: String -> Char -> [String]
partes [] _ = [[]]
partes (x:xs) n q
 -}

-- 4
data BTree a = Empty | Node a (BTree a) (BTree a)
a1 =                           Node 5 
            (Node 3 Empty Empty) (Node 7 Empty 
                                      (Node 9 Empty Empty)) 
--a 
minWMin :: Ord a => BTree a -> (a, BTree a) -- does a and b in one go
minWMin Empty = error "No Empty Trees"
minWMin (Node n Empty _) = (n, Empty)
minWMin (Node n e d) = (e1, Node n e2 d) 
   where (e1,e2) = minWMin e

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove n (Node n' e d)
 | n < n' = Node n' (remove n e) d
 | n > n' = Node n' e (remove n d)
 | otherwise = case d of
  Empty -> e
  _     -> Node m e d
  where
  (m,d') = minWMin d
             
-- b
instance (Show a) => Show (BTree a) where
    show (Empty) = "*"
    show (Node a l r) = "(" ++ show l ++ " <-" ++ show a ++ "->" ++ show r ++ ")"

      
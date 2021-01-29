module BinaryTrees where

data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show

a1 = Node 4 (Node 3 (Node 1 Empty Empty) (Node 5 Empty Empty)) (Node 6 Empty (Node 9 Empty Empty))
{- 
                4
              /  \
            3     6
          /  \     \
         1    5     9
 -}
--1
--a
height :: BTree a -> Int -- Tree's height
height Empty        = 0
height (Node n e d) = 1 + max (height e) (height d )


-- b
countNodes :: BTree a -> Int -- Number of nodes
countNodes Empty        = 0
countNodes (Node n e d) = 1 + (+) (countNodes e) (countNodes d)

-- c
leaves :: BTree a -> Int -- Number of Leaves (i.e. nodes without descendants)
leaves Empty                = 0 
leaves (Node _ Empty Empty) = 1
leaves (Node n e d)         = leaves e + leaves d

-- d
prune :: Int -> BTree a -> BTree a -- removes all nodes from a given depth
prune 0 _            = Empty
prune 1 (Node n _ _) = Node n (Empty) (Empty)
prune a (Node n e d) = Node n (prune(a-1) e) (prune (a-1) d)

--e 
path :: [Bool] -> BTree a -> [a] -- given a path(False-> Left; True -> Right)and a Tree, provides a list with every node that path passes through
path _ Empty = []
path [] (Node n _ _) = [n]
path (x:xs) (Node n e d) = if x then n: path xs d else n:path xs e

a2 = Node 4 (Node 3 (Node 1 Empty Empty) (Node 5 Empty Empty)) (Node 6 (Node (-4) Empty Empty ) (Node 9 Empty Empty))
{- 
                4
              /  \
            3     6
          /  \   /  \
         1    5 -4  9
 -}
--f 
mirror :: BTree a -> BTree a -- symmetric Tree
mirror Empty = Empty
mirror (Node n e d ) = Node n (mirror d) (mirror e)

-- g 

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b ->BTree c
zipWithBT _ Empty Empty = Empty
zipWithBT f (Node na ea da) (Node nb eb db) = Node (f na nb) (zipWithBT f ea eb) (zipWithBT f da db)  

--f
unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) e d) = (Node a ea da, Node b eb db, Node c ec dc)
 where 
   (ea, eb, ec) = unzipBT e
   (da,db,dc) = unzipBT d

-- 2
--a 
minimumST :: Ord a => BTree a -> a -- returns minimum value
minimumST Empty = error "No empty Trees"
minimumST (Node n Empty _) = n
minimumST (Node _ e _) = minimumST e 

-- b
noMinimum :: Ord a => BTree a -> BTree a -- removes minimum value
noMinimum Empty = error "No empty Trees"
noMinimum (Node n Empty _) = Empty
noMinimum (Node n e d) = Node n (noMinimum e) d

-- c 
minWMin :: Ord a => BTree a -> (a, BTree a) -- does a and b in one go
minWMin Empty = error "No Empty Trees"
minWMin (Node n Empty _) = (n, Empty)
minWMin (Node n e d) = (e1, Node n e2 d) 
   where (e1,e2) = minWMin e

-- d
{- removeSearchT :: Ord a => a -> BTree a -> BTree a 
removeSearchT Empty = error "No Empty Trees"
removeSearchT r (Node n Empty Empty) = if r == n then   -}
remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove n (Node n' e d)
 | n < n' = Node n' (remove n e) d
 | n > n' = Node n' e (remove n d)
 | otherwise = case d of
  Empty -> e
  _     -> Node m e d'
  where
  (m,d') = minWMin d
  -- TO BE REVISED

-- 3

type Student = (Number, Name, Regime, Grade)
type Number = Int
type Name = String
data Regime = ORD | TE | MEL deriving Show
data Grade = Aprov Int
           | Fail
           | Skipped
          deriving Show
type Class = BTree Student -- Binary Search Tree (FIltered by number)

-- a
inscNum :: Number -> Class -> Bool -- verifies if a student is registered
inscNum _ Empty = False
inscNum x (Node (n,_,_,_) e d) = n == x || inscNum x (if x < n then e else d)

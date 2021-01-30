module MoreTrees where

-- 1 Expressions
data ExpInt = Const Int
            | Sim ExpInt
            | Sum ExpInt ExpInt
            | Sub ExpInt ExpInt
            | Mult ExpInt ExpInt

-- ExpInt's terms can be seen as trees whose leafs are 
-- integers and whose nodes (non leaves) are operators
t1 = (Sum (Const 3) (Sub (Const 9) (Mult (Const 5) (Const 2))))
{- 
               Sum 
             /    \
        Const 3     Sub
                   /   \
              Const 9  Mult
                     /      \
                 Const 5   Const 2
  -}
--a 
calculateExpInt :: ExpInt -> Int --given one of the expressions calculates its value
calculateExpInt er = case er of
 Const n -> n
 Sim e -> - calculateExpInt e
 Sum e1 e2 -> (calculateExpInt e1) + (calculateExpInt e2) 
 Sub e1 e2 -> (calculateExpInt e1) - (calculateExpInt e2) 
 Mult e1 e2 -> (calculateExpInt e1) * (calculateExpInt e2) 

-- b
infixExpInt :: ExpInt -> String
infixExpInt exp = case exp of
  Const n    -> show n
  Sim e1     -> "-(" ++ infixExpInt e1 ++ ")"
  Sum e1 e2  -> "("  ++ infixExpInt e1 ++ " + " ++ infixExpInt e2 ++ ")"
  Sub e1 e2  -> "("  ++ infixExpInt e1 ++ " - " ++ infixExpInt e2 ++ ")"
  Mult e1 e2 -> "("  ++ infixExpInt e1 ++ " * " ++ infixExpInt e2 ++ ")"
  
-- c 
posfixExpInt :: ExpInt -> String
posfixExpInt exp = case exp of
  Const n    -> show n
  Sim e1     -> posfixExpInt e1 ++ " " ++ " - "
  Sum e1 e2  -> posfixExpInt e1 ++ " " ++ posfixExpInt e2 ++ " + "
  Sub e1 e2  -> posfixExpInt e1 ++ " " ++ posfixExpInt e2 ++ " - "
  Mult e1 e2 -> posfixExpInt e1 ++ " " ++ posfixExpInt e2 ++ " * "

  --2 rose trees
    
data RTree a = R a [RTree a] deriving Show
{- 
           R 5
         /    \
       [ 2     9 ]
      |  \      |
     [ 2  4 ]   []
      |   |
      [] []

 -}
rt1 = R 5 [R 2 [R 2 [], R 4 []], R 9 []]
-- a 
sumRT :: Num a => RTree a -> a -- sums the elements
sumRT (R a []) = a
sumRT (R a l1) = (+) a (sum(map sumRT l1))

-- b
heightRT :: RTree a -> Int -- calculates the tree's height
heightRT (R _ []) = 1
heightRT (R _ l1) = 1 + maximum(map heightRT l1)

-- c
pruneRT :: Int -> RTree a -> RTree a -- removes all elements after given depth
pruneRT 0 (R n _) = R n []
pruneRT x (R n l1) = R n (map (pruneRT (x-1)) l1)

-- d 
mirrorRT :: RTree a -> RTree a -- generates the symmetrical tree
mirrorRT (R n l1) = R n (reverse(map mirrorRT l1))

-- e 
postorder :: RTree a -> [a]
postorder 
module MoreTrees where

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


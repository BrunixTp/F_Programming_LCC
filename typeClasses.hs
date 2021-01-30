module TypeClasses where

--1
data Frac = F Integer Integer
-- a
normalize :: Frac -> Frac
normalize (F a b) = F (div a c) (div b c)
 where
     c = mdc a b
mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc x y = mdc y (mod x y)

-- b
instance Eq Frac where
    (F x y) == (F a b) = compareAux (normalize (F x y)) (normalize (F a b))
      where 
       compareAux (F x1 y1) (F x2 y2) = x1 == x2 && y1 == y2

instance Ord Frac where
  compare (F x y) (F x1 y1)
   | c1 < c2 = LT
   | c1 == c2 = EQ
   | c1 > c2 = GT
    where
     c1 = (fromIntegral x)/(fromIntegral y)
     c2 = (fromIntegral x1)/(fromIntegral y1)

instance Show Frac where
    show (F x y) = show x ++ "/" ++ show y

instance Num Frac where
    

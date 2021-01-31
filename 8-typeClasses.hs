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
  (+) = sumFrac
  (*) = multFrac
  negate (F x y) = F (-x) y
  abs (F x y) = F (abs x) (abs y)
  signum (F x y ) = normalize (F (signum x) (signum y))
  fromInteger x = F x 1
sumFrac :: Frac -> Frac -> Frac
sumFrac (F a b) (F x y) = F ((a*y) + (x*b)) (y*b)
multFrac :: Frac -> Frac -> Frac
multFrac (F a b) (F x y) = F (a*x) (b*y)

-- f
selfrac :: Frac -> [Frac] -> [Frac]
selfrac f = filter (\x -> f*(fromInteger 2) < x)

-- 2
--a
data Exp a = Const a 
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)
{- 
instance Show Exp where
  Show (Const a) = show a -}
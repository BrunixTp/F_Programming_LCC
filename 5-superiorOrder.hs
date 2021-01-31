module SuperiorOrder where

-- a
qualquer :: (a -> Bool) -> [a] -> Bool 
qualquer _ [] = False 
qualquer funcao (x:xs) = funcao x || qualquer funcao xs
 
-- b
zipCom :: (a->b->c) -> [a] -> [b] -> [c]
zipCom _ [] _ = []
zipCom _ _ [] = []
zipCom funcao (x:xs) (y:ys) = funcao x y: zipCom funcao xs ys

-- c
takeEnq :: (a -> Bool) -> [a] -> [a]
takeEnq _ [] = []
takeEnq funcao (x:xs) = if funcao x then x: takeEnq funcao xs else []

-- d 
dropEnq :: (a-> Bool) -> [a] -> [a]
dropEnq _ [] = []
dropEnq f (x:xs) = if f x then dropEnq f xs else x:xs

-- e 
ambosEnq :: (a -> Bool) -> [a] -> ([a],[a])
ambosEnq _ [] = ([],[])
ambosEnq f (x:xs) = if f x then (x:a,b) else ([],x:xs)
 where (a,b) = ambosEnq f xs

-- f
apagaPor :: (a -> a -> Bool) -> a -> [a] -> [a]
apagaPor _ _ [] = []
apagaPor f n (x:xs) = if f n x then xs else x: apagaPor f n xs

-- g 
ordenaOn :: Ord b => (a -> b) -> [a] -> [a] 
ordenaOn _ [] = []
ordenaOn f (x:xs) = inserirOn f x (ordenaOn f xs)
inserirOn :: Ord b => (a -> b) -> a -> [a] -> [a]
inserirOn _ a [] = [a]
inserirOn f n (x:xs) | f n <= f x = n:x:xs
                     | otherwise = x: inserirOn f n xs

-- 3
type Mat a = [[a]]

{- 
[ 1 2 3  ]
| 0 4 5  |
[ 0 0 6  ]
 [[1,2,3], [0,4,5], [0,0,6]]
 -}

-- a
dimOK :: Mat a -> Bool 
dimOk [[]] = False
dimOK (l:ls) = all (\l' -> length l' == length l) ls

-- b

dimMat :: Mat a -> (Int, Int)
dimMat [] = (0,0)
dimMat (l:ls) = if dimOK (l:ls) then (length l, length (l:ls))
  else error "Not well defined"

-- c

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat = zipWith(zipWith (+))

-- d 

transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose l = map head l: transpose(map tail l)

-- e 

multMat :: Num a => Mat a -> Mat a -> Mat a 
multMat m1 m2 = [[sum(zipWith(*) l1 l2) | l2 <- transpose m2] | l1 <- m1]

-- f
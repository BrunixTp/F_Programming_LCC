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


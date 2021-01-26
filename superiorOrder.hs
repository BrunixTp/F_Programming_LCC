module SuperiorOrder where

qualquer :: (a -> Bool) -> [a] -> Bool 
qualquer _ [] = False 
qualquer funcao (x:xs) = funcao x || qualquer funcao xs
 
zipCom :: (a->b->c) -> [a] -> [b] -> [c]
zipCom _ [] _ = []
zipCom _ _ [] = []
zipCom funcao (x:xs) (y:ys) = funcao x y: zipCom funcao xs ys

takeEnq :: (a -> Bool) -> [a] -> [a]
takeEnq _ [] = []
takeEnq funcao (x:xs) = if funcao x then x: takeEnq funcao xs else []

import Data.List
import Data.Char
import Data.Either
import Data.Maybe
-- First Exercise

oneToOther :: Int -> Int -> [Int]
oneToOther x y
 | x < y  = x:oneToOther (x+1) y
 | otherwise = []

-- Second Exercise

fromOneToOther :: Int -> Int -> Int -> [Int]
fromOneToOther x y z 
 | x <= y && y <= z   =  x : fromOneToOther y (y + (y-x)) z 
 | x >= y && y >= z   =  x : fromOneToOther y (y - (x-y)) z
 | otherwise = [x]

-- Third Exercise

joinList :: [a] -> [a] -> [a]
joinList l1 [] = l1
joinList [] l2 = l2
joinList (x:xs) l2 = x : joinList xs l2

-- Fourth Exercise
indiceLista :: [a] -> Int -> a
indiceLista [] _     = error "index too large"
indiceLista (x:_) 0 = x
indiceLista (_:xs) n = indiceLista xs (n-1)

-- Fifth Exercise 

inverso :: [a] -> [a]
inverso [] = []

inverso l = inverso (tail l) ++ [head l]

-- Sixth Exercise

nValuesOfList :: Int -> [a] -> [a]
nValuesOfListist _ [] = []
nValuesOfList 0 _ = []
nValuesOfList x (h:t) = h :  nValuesOfList (x-1) t


-- Seventh Exercise

ignore :: Int -> [a] -> [a]
ignore 0 l      = l
ignore _ []     = []
ignore n (x:xs) = if n >= length (x:xs) 
    then [] 
    else ignore (n-1) xs 


-- Eigth Exercise

comprimir :: [a] -> [b] -> [(a,b)]
comprimir [] _ = []
comprimir _ [] = []
comprimir (a:as) (b:bs) = (a,b) : comprimir as bs

-- Nine Exercise

pertence :: Eq a => a -> [a] -> Bool
pertence _ []     = False
pertence n (x:xs) = x == n || pertence n xs


-- Tenth Exercise

replicar :: Int -> a -> [a] 
replicar 0 _ = []
replicar x y = y : replicar (x-1) y 

-- Eleventh Exercise

mediate :: a -> [a] -> [a]
mediate _ [] = []
mediate x (h:t) = if length (h:t) == 1
    then [h] 
    else h : x : mediate x t 

-- Twelfth Exercise (Not DONE)

agrupar :: Eq a => [a] -> [[a]]
agrupar [] = []
agrupar (x:xs) = agrupa [x] xs
  where
    agrupa a [] = [a]
    agrupa a (y:ys)
      | y == head a = agrupa (a ++ [y]) ys
      | otherwise   = a : agrupa [y] ys 

-- Thirteenth Exercise
-- inverso de form_party
ungroup :: [[a]] -> [a]
ungroup []    = []
ungroup (h:t) = ungroup_loop h t
 where 
 ungroup_loop acc []      = acc
 ungroup_loop acc (h2:t2) = ungroup_loop (acc ++ h2) t2

-- Fourteenth Exercise

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l  = inits' (init l) ++ [l]

-- Fifteenth Exercise
-- inverso de indices

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l  = l :tails' (tail l)

-- Sixteenth Exercise 
primeirosPertencem :: Eq a => [a] -> [a] -> Bool
primeirosPertencem [] l1         = True
primeirosPertencem l1 []         = False
primeirosPertencem (x:xs) (y:ys) = x == head ys && primeirosPertencem xs ys

-- Seventeenth 

endsIn :: Eq a => [a] -> [a] -> Bool
endsIn [] _         = True
endsIn _ []         = False
endsIn (x:xs) (y:ys) = (x /= y) && endsIn xs ys

-- Eighteen

isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' _ []         = False
isSubsequenceOf' [] _         = True
isSubsequenceOf' (x:xs) (y:ys) = if x == y
    then isSubsequenceOf' xs ys
    else isSubsequenceOf' (x:xs) ys

-- Nineteen

indicesAux:: Eq a=> a->[a]->Int->[Int]
indicesAux _ [] _ = []
indicesAux x (h:t) n
    | x == h    = n: indicesAux x t (n+1)
    | otherwise = indicesAux x t (n+1)

elemIndices':: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x l  = indicesAux x l 0

-- Twenty

noRepeat :: Eq a => [a] -> [a]
noRepeat []     = []
--noRepeat [x] = [x]
noRepeat (x:xs) = x : noRepeat (noRepeatAux x xs)

noRepeatAux :: Eq a => a -> [a] -> [a]
noRepeatAux _ []       = []
noRepeatAux var (x:xs) = if var == x then noRepeatAux var xs
 else x: noRepeatAux var xs

-- Twenty One 

remove :: Eq a => a -> [a] -> [a]
remove x []    = []
remove x (h:t) = if x == h then t
 else h: remove x t

-- Twenty Two 
 
removeLists :: Eq a => [a] -> [a] -> [a]
removeLists [] _     = []
removeLists l1 []     = l1
removeLists (h:t) (x:xs) = removeLists(remove x xs)


-- revisit twenty two 
-- Twenty Three

uniao :: Eq a => [a] -> [a] -> [a]
uniao l1 l2 = l1 ++ removeListsImp l2 l1
 where removeListsImp [] _ = []
       removeListsImp l1 [] = l1
       removeListsImp (h:t) (x:xs) = if pertence h (x:xs) then removeListsImp t (x:xs)
                                     else h:removeLists t (x:xs)

{-
uniao :: Eq a => [a] -> [a] -> [a]
uniao l [] = l
uniao [] l = l
uniao (x:xs) (y:ys) = if x == y then x: uniao xs (ys)
 else x: uniao xs (y:ys)
-}

{-
uniao [1,2,3] [2,4] ++ []
uniao [1,2,3] [2,4] ++ []
uniao [1,2,3] [2,4] ++ [4]
uniao [1,2,3,4] [2,4] ++ []
[1,2,3,4]
-}

-- Twenty Four

inter :: Eq a => [a] -> [a] -> [a]
-- inter [1,2,3] [3] = [3]
-- se n houver repetidos -> l2 
inter _ []          = []
inter [] _          = []
inter (x:xs) (y:ys) = interAux (x:xs) y ++ inter (xs) ys

interAux :: Eq a => [a] -> a -> [a]
interAux (x:xs) n
 | x == n = x : interAux xs n
 | otherwise = interAux xs n
interAux _ _  = []

-- Revisit Maybe
-- twenty five
inserir :: Ord a => a -> [a] -> [a]
inserir n []    = [n]
inserir n (h:t) = if n <= h then n : (h:t) else h: inserir n t

-- Twenty six

sentence :: [String] -> String
sentence []     = "" 
sentence (x:xs) = x ++ " " ++ sentence xs

-- Twenty seven

unlines' :: [String] -> String
unlines' []     = "" 
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

-- Twenty eight

pmaior :: Ord a => [a] -> Int
pmaior l1 = snd (pmaiorAux l1)

pmaiorAux :: Ord a => [a] -> (a,Int)
pmaiorAux [x] = (x,0) 
pmaiorAux l1  = if head l1 > m then (head l1,0) else (m,p+1)
 where (m,p)  = pmaiorAux (tail l1) 

{-
pmaior [1,2,3]
snd (pmaiorAux [1,2,3,4]) -> (1 > m) -> (1 > pmaiorAux [2,3,4]) -> (1 > (2 > pmaiorAux[3,4]))
-> (1 > (2 > (3 > (pmaiorAux [4])))) ->(1 > (2 > (3 > (4,0)))) -> (1 > (2 > (4,1))) -> (1 > (4,2)) -> (4,3)
snd (4,3) = 3
-}
-- Twenty nine

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos l1 = temRepetidosAux (head l1) l1> 1 || temRepetidos (tail l1)

temRepetidosAux :: Eq a => a -> [a] -> Int
temRepetidosAux _ []     = 0
temRepetidosAux x (y:ys) = if x == y then k+1
 else k
 where k = temRepetidosAux x (ys)


-- Thirty 
-- só permite números

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (x:xs) 
 | isDigit x = x : algarismos xs --x >= '0' && x <= '9'
 | otherwise = algarismos xs

-- Thirty One

posImpares :: [a] -> [a]
posImpares l1 = posImparesAux l1 0
 where 
     posImparesAux [] _  = []
     posImparesAux (h:t) x 
         | odd x  = h : posImparesAux t (x+1)
         | otherwise = posImparesAux t (x+1)

-- Thirty Two 

posPares :: [a] -> [a]
posPares l1 = posParesAux l1 0
 where
     posParesAux [] _ = []
     posParesAux (h:t) x
        | even x = h : posParesAux t (x+1)
        | otherwise = posParesAux t (x+1)

-- Thirty Three

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- Thirty Four 

iSort :: Ord a => [a] -> [a]
iSort []    = []
iSort (h:t) = insert h t'
    where t' = iSort t
{- 
inserir :: Ord a => a -> [a] -> [a]
inserir n []    = [n]
inserir n (h:t) = if n <= h then n : (h:t) else h: inserir n t
-}

-- Thirty Five
menor :: String -> (String -> Bool)
menor [] []= False
menor [] _ = True
menor _ [] = False
menor (x:xs) (y:ys) | x == y = menor xs ys
                    | x > y  = False
                    | x < y  = True


-- Thirty Six
-- tipo [(a,Int)]
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet x (h:t) = x == fst h || elemMSet x t

-- Thirty Seven
lengthMSet :: [(a, Int)] -> Int
lengthMSet []    = 0
lengthMSet (h:t) = snd h + lengthMSet t


-- Thirty Eight
converteMSet :: [(a, Int)] -> [a]
converteMSet [] = []
converteMSet (h:t) = converteMSetAcc (snd h) (h:t)
 where converteMSetAcc _ [] = []
       converteMSetAcc acc (h:t) | acc > 1  = fst h: converteMSetAcc (acc-1) (h:t)
                                 | acc == 1 = fst h: converteMSet t

-- Thirty Nine
insereMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)] 
insereMSet x [] = [(x,1)]
insereMSet x l1 = insereMSetAux x l1
 where insereMSetAux _ [] = []
       insereMSetAux x (h:t) | x == fst h  = (fst h,snd h + 1):t
                             | otherwise = h:insereMSet x t


-- Forty
removeMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet x l1 = removeMSetAux x l1
 where removeMSetAux _ [] = []
       removeMSetAux x (h:t) | x == fst h = if snd h -1 == 0 then t else (fst h, snd h - 1):t  
                             | otherwise  = removeMSet x t


-- Forty One

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = iSort(adiciona h (constroiMSet t))
 where adiciona x [] = [(x,1)] 
       adiciona x ((y,s):as) | x == y  = as ++ [(y, s +1)]
                             | x /= y = (y,s): adiciona x as
                             

-- fourty two
pEithers :: [Either a b] -> ([a],[b])
pEithers [] = ([],[])
pEithers (x:xs) = case x of
    Left y  -> (y:ls, rs)
    Right y -> (ls, y:rs)
    where (ls, rs) = pEithers xs 

-- fourty three
catMaybe' :: [Maybe a] -> [a]
catMaybe' [] = []
catMaybe' (h:t) = case h of 
    Just y  -> y : catMaybe' t
    Nothing -> catMaybe' t

-- fourty four
data Movimento = Norte | Sul | Este | Oeste
                    deriving Show

posicao ::  (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (h:t) = case h of
    Norte -> posicao (x,y+1) t
    Sul -> posicao (x,y-1) t
    Este -> posicao (x+1,y) t
    Oeste -> posicao (x-1,y) t

-- fourty five

caminho ::  (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (w,z) | x == w && y == z = [] 
                    | x > w            = Oeste: caminho (x-1,y) (w,z)
                    | x < w            = Este: caminho (x+1,y) (w,z)
                    | y > z            = Sul: caminho (x,y-1) (w,z)
                    | otherwise        = Norte: caminho (x,y+1) (w,z)

-- fourty six
vertical ::  [Movimento] -> Bool
vertical [] = True
vertical (h:t) = case h of
    Este -> False
    Oeste -> False
    Norte -> vertical t 
    Sul -> vertical t 

-- forty seven 
data Posicao = Pos Int Int
 deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [] = error "empty list"
maisCentral [x] = x
maisCentral ((Pos x1 y1):(Pos x2 y2):xs)
  | (x1^2 + y1^2) >= (x2^2 + y2^2) = maisCentral ((Pos x2 y2):xs)
  | otherwise                      = maisCentral ((Pos x1 y1):xs)

-- forty eight
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x1 y1) ((Pos x2 y2):t)
 | (x2 == x1 -1) && (y1 == y2) || (x2 == x1 +1) && (y1 == y2) || (y2 == y1 +1) && (x1 == x2) || (y2 == y1 -1) && (x1 == x2) = (Pos x2 y2): vizinhos (Pos x1 y1) t
 | otherwise = vizinhos (Pos x1 y1) t

-- forty nine

mesmaOrdenada:: [Posicao]->Bool
mesmaOrdenada [] = True
mesmaOrdenada [_] = True
mesmaOrdenada ((Pos _ y1):(Pos _ y2):t)
    | y1 /= y2 = False
    | otherwise = mesmaOrdenada t
-- fifty

data Semaforo = Verde | Amarelo | Vermelho
                      deriving Show

intersecaoOK :: [Semaforo] -> Bool
intersecaoOK l = contaNaoVermelhos l <= 1 
   where contaNaoVermelhos :: [Semaforo] -> Int
         contaNaoVermelhos [] = 0
         contaNaoVermelhos (Vermelho:t) = contaNaoVermelhos t 
         contaNaoVermelhos (_:t) = 1 + contaNaoVermelhos t

interseccaoOk1 :: [Semaforo] -> Bool
interseccaoOk1 [] = False
interseccaoOk1 (_:Vermelho:_) = True
interseccaoOk1 (x:xs) = case x of
  Verde -> interseccaoOk1 xs
  Amarelo -> interseccaoOk1 xs
  Vermelho -> True 
module Cinq where

import Data.Char


--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x s
  | x > s = []
  | otherwise = x : enumFromTo' ( x+ 1) s

--2 
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' start next end
    | start > end && next - start > 0 || start < end && next - start < 0 = []
    | otherwise = start:enumFromThenTo' next (2 * next - start) end

--3
pp :: [a] -> [a] -> [a]
pp [] l = l
pp (xs:x) l = xs:pp x l

--4
el :: [a] -> Int -> a
el (x:h) t
   | t == 0 = x
   | otherwise = el h (t-1)

--5 NÃO ENTENDI
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:h) = reverse' h ++ [x]

--6
take' :: Int -> [a] -> [a]
take' n [] = []
take' n (x:l) 
 |n <= 0 = []
 | otherwise = x : take' (n-1) l

-- 7
drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' n [] = []
drop' n (x:l) 
 | n < 0 = x:l
 | otherwise = drop' (n-1) l

-- 8
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:l) (y:ys) = (x,y) : zip' l ys

-- 9 DÁ STRING EM VEZ DE LISTA
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x 
      |n < 0 = []
      | otherwise = x : replicate' (n-1) x

-- 10 
intersperse' :: a -> [a] -> [a]
intersperse' n [] = []
intersperse' n (x:t) = x : n : intersperse' n t

-- 11 TENHO DÚVIDAS
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [x] = [[x]]
group' (x:t) 
 | elem x (head r) = (x : (head r)) : tail r
 | otherwise = [x] : r
 where r = group' t

group'' :: Eq a => [a] -> [[a]]
group'' [] = []
group'' [x] = [[x]]
group'' (h1:h2:t) 
 | h1 == h2 = (h1: head gt) : (tail gt)
 | otherwise = [h1] : gt
 where gt = group'' (h2 :t)

-- 12
concat' :: [[a]] -> [a]
concat' [] = []
concat' [[x]] = [x]
concat' (x:xs) = x ++ concat' xs

-- 13 NÃO ENTENDI
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l  = inits' (init l) ++ [l]

-- 14 !! ATENÇÃO NA ORDEM
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = [l] ++ tails' (tail l) 

-- 15
heads :: [[a]] -> [a]
heads [] = []
heads ([]:l) = heads l
heads (x:l) = head x : heads l

--16
total :: [[a]] -> Int
total [] = 0
total (x:t) = length x + total t

--17 (VER)
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((x,y,z):t) = (x,z) : fun t 

-- 18
cola :: [(String,b,c)] -> String
cola [] = ""
cola ((x,y,z): t) = x ++ cola t

--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade a i [] = []
idade a i ((n,b):t) 
 | a-i < b = idade a i t
 | otherwise = n : idade a i t

-- 20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 0 = [1]
powerEnumFrom n m = n^m : powerEnumFrom n (m-1)

--21 ANALISAR MELHOR
isPrime :: Int -> Bool
isPrime n 
 | n >= 2 = primeAux n 2
 | otherwise = False

primeAux :: Int -> Int -> Bool
primeAux n m 
 | m*m > n = True
 | mod n m == 0 = False
 | otherwise = primeAux n (n+1)

-- 22 
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:t) (y:ys) = x==y && isPrefixOf' t ys

--23 (DÚVIDAS NO RACIOCÍNIO)
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' x l@(y:ys) = x == l || isSuffixOf' x ys

--24 (DUVIDAS NO RACIOCÍNIO)
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (x:xs) (y:ys) = x == y && isSubsequenceOf' xs ys || isSubsequenceOf' (x:xs) ys

--25
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' x l = elemAux x l 0

elemAux :: Eq a => a -> [a] -> Int -> [Int]
elemAux _ [] _ = []
elemAux x (h:t) i 
 | x == h = i : elemAux x t (i+1)
 | otherwise = elemAux x t (i+1)

--26
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t)
 | h `elem` t = nub' t
 | otherwise  = h : nub' t

--27
delete' :: Eq a => a -> [a] -> [a] 
delete' _ [] = []
delete' x (h:l)
 | x == h = l
 | otherwise = h : delete' x l

--28 REVER COM CALMA
uu :: Eq a => [a] -> [a]-> [a]
uu [] _ = []
uu l [] = l
uu l (x:h) = uu (uuAux x l) h

uuAux :: Eq a => a -> [a]-> [a]
uuAux x (y:t) 
 |x == y = t
 | otherwise = y : uuAux x t

--29 
union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l 
union' [] l = l
union' l (x:h)
 | x `elem` l = union' l h 
 | otherwise = union' (l ++ [x]) h

--30
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' l [] = [] 
intersect' [] _ = []  
intersect' (x:h) l 
 | x `elem` l = x : intersect' h l 
 | otherwise = intersect' h l 

-- 31
insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (x:t) 
 | n > x = x : insert' n t
 | otherwise = n:x:t

--32
unwords' :: [String] -> String
unwords' [] = ""
unwords' (x:h) = x ++ (if null h then "" else " " ) ++ unwords' h

--33 
unlines' :: [String] -> String
unlines' [] = ""
unlines' (x:t) = x ++ "\n" ++ unlines' t

--34
pMaior :: Ord a => [a] -> Int
pMaior [] = 0
pMaior l = pMaiorAux l 0 

pMaiorAux :: Ord a => [a] -> Int -> Int
pMaiorAux (h:t) i 
 | h == (maximum (h:t)) = i
 | otherwise = pMaiorAux t (i+1)

-- 35
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' p [] = Nothing
lookup' p ((x,y):t)
 | p == x = Just y
 | otherwise = lookup' p t

--36
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:m:t) 
 | h < m = h : m : preCrescente t 
 | otherwise = [h] 

--37 
iSort :: Ord a => [a] -> [a]
iSort [] = []
isort [x] = [x]
isort (x:t) = insert' x (isort t)

-- 38
menor :: String -> String -> Bool
menor (h:t) (p:s)
 | h > p = False
 | h == p = menor t s 
 | otherwise = True

-- 39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet e ((x,y):t) 
  | e == x || elemMSet e t = True
  | otherwise = False

--40
convertMSet :: [(a,Int)] -> [a]
convertMSet [] = []
convertMSet ((x,y):t) = convertAux x y ++ convertMSet t

convertAux :: a -> Int -> [a]
convertAux x 0 = []
convertAux x y = x : convertAux x (y-1)

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet c [] = []
insereMSet c ((x,y):h) 
 | c /= x = (x,y) : insereMSet c h 
 | otherwise = (c,y+1) : h

--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet c ((x,y):t)
 | c == x = if y>1 then (x,y-1):t else t
 | otherwise = (x,y) : removeMSet c t

--43 VER
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet [x] = [(x,1)]
constroiMSet (x:z:t) 
 | x == z = constroiMSetAux x t 2 : constroiMSet( constroiMSet' x t)
 | otherwise = (x,1) : constroiMSet (z:t)

constroiMSetAux :: Eq a => a -> [a] -> Int -> (a,Int)
constroiMSetAux x [] i = (x,i)
constroiMSetAux x (h:t) i
 | x == h = constroiMSetAux x t (i+1) 
 | otherwise = (x,i)

constroiMSet' :: Eq a => a -> [a] -> [a]
constroiMSet' x [] = []
constroiMSet' x (h:t)
 | x == h =  constroiMSet' x t
 | otherwise = (h:t)

--44
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left a): t) = (a: b,c)
           where (b,c) = partitionEithers' t 
partitionEithers' ((Right a): t) = (b,a: c)
           where (b,c) = partitionEithers' t 

--45
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Just x):t) = x : catMaybes' t
catMaybes' (Nothing : t) = catMaybes' t

--46

data Movimento = Norte | Sul | Este | Oeste deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (a,b) 
 | y < b = Norte : caminho (x , y +1) (a,b)
 | y > b = Sul : caminho (x,y-1) (a,b)
 | x > a = Oeste : caminho (x-1,y) (a,b)
 | x < a = Este : caminho (x+1,y) (a,b)
 |otherwise = []

--47
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p 
posicao (x,y) (Norte:t) = posicao (x, y+1) t
posicao (x,y) (Sul:t) = posicao (x, y-1) t
posicao (x,y) (Este:t) =  posicao (x+1, y) t
posicao (x,y) (Oeste:t)  =  posicao(x-1, y) t


hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops posi movs = posi == posicao posi movs || hasLoops posi (init movs)

--48
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (x,y) (a,b)):t) 
 | abs(b-y) == abs(a-x) = 1 + contaQuadrados t 
 | otherwise = contaQuadrados t

--49
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal (Rect (x,y) (a,b):t) = abs(a-x)*abs(b-y) + areaTotal t 

--50

data Equipamento = Bom | Razoavel | Avariado deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom: t) = 1 + naoReparar t
naoReparar (Razoavel:t) =1 + naoReparar t 
naoReparar (Avariado:t) = naoReparar t











{- removeAux :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeAux c [] = []
removeAux c ((x,y):t)
 | c /= x = (x,y) : removeAux c t 
 | otherwise = removeAux c t -}




{-elemMSet e ((x,y):t)
 | y < 0 = False
 | x == fst (head t) = False
 | e = True
 | otherwise = False
 where r = elemMSet t -}






{-pMaior :: [Int] -> Int
pMaior (x:z:h) 
 | x > pMaior (z:h) = 0
 | otherwise = pMaiorAux h z 0 

pMaiorAux :: [Int] -> Int -> Int -> Int
pMaiorAux (x:h) n i 
 | x >= n = i + (pMaiorAux h n (i+1))
 | otherwise = pMaiorAux h n (i+1) --}
      


-- [right"5", left 3, right "5", right "6", left "4"]

{- partitionEither' :: [Either a b] -> ([a],[b])
partitionEither' [] = ([],[])
partitionEither' ((Left a):t) = (a:b,c)
                     where (b,c) =  partitionEither' t 
partitionEither' ((Right a):t) = (b,a:c)
                     where (b,c) = partitionEither' t -}

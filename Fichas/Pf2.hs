module Pf2 where

import Data.Char

-- 1 a)
{-}>>> funA [2,3,5,1]
    funA [] = 0 
    funA 2:[3,5,1] = 2^2 + funA [3,5,1]
    funA 3: [5,1] = 3² + funA [5,1]
    funA 5:[1] = 5^2 + funA [1]
    funA 1:[] = 1^2 + funA []
      = 4+9+25+1 = 39 -}

--2 a)
dobros :: [Float] -> [Float] 
dobros [] = []
dobros (y:ys) = y*2 : dobros ys 

-- b)
numOcorre :: Char -> String -> Int 
numOcorre _ "" = 0
numOcorre x (a:s) = if x == a
                     then 1 + numOcorre a s
                      else 0 + numOcorre a s 

-- c)
positivos :: [Int] -> Bool
positivos [x] = x > 0 
positivos (x:xs) 
 | x <= 0 = False
 | otherwise = positivos xs 

--d)
soPos :: [Int] -> [Int] 
soPos [] = []
soPos (x:xs) 
 | x < 0 = soPos xs
 | otherwise = x : soPos xs

-- e)
somaNeg :: [Int] -> Int 
somaNeg [] = 0
somaNeg (x:xs) 
 | x < 0 = x + somaNeg xs
 | otherwise = somaNeg xs

-- f)
{-tresUlt :: [a] -> [a] 
tresUlt [] = []
tresUlt (x:xs) 
 | length(x:xs) < 3 = (x:xs)
 | length (x:xs) == 3 = (x:xs)
 | otherwise =  tresUlt (x:xs)-}

tresUlt :: [a] -> [a]
tresUlt (x:y:z:[]) = [x,y,z]
tresUlt (x:y:z:xs) = (y:z:xs)
tresUlt l = l

-- h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros u [] = False
nosPrimeiros u ((x,y): xs) 
 | u == x = True 
 | otherwise = nosPrimeiros u xs

 -- i)
 {- sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c) 
 sumTriplos [] = (0,0,0)
 sumTriplos ((x,y,z): t) =
  let (a,b,c) = sumTriplos t 
  in (x+a,y+b,z+c) -}

-- 3 a)
soDigitos :: [Char] -> [Char] 
soDigitos [] = []
soDigitos (h:t) 
 | isDigit h = h : soDigitos t 
 | otherwise = soDigitos t 

-- c)
nums :: String -> [Int]
nums "" = []
numa (h:t) 
 | isDigit h = digitToInt h : nums t
 | otherwise = nums t

-- 4 a)

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((x,y):t) 
 | y == n = 1 + conta n t 
 |otherwise = conta n t

-- b)
grau :: Polinomio -> Int
grau [] = 0
grau ((x,y): t) 
 | y >= grau t = y
 | otherwise = grau t 
 
-- c) 
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau g ((x,y):xs) 
 | g == y = (x,y) : selgrau g xs
 | otherwise = selgrau g xs

-- d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,y):xs) 
 | y == 0 = (0,0): deriv xs
 | otherwise = (x*fromIntegral y, y-1) : deriv xs

 -- e)
calcula' :: Float -> Polinomio -> Float 
calcula' _ [] = 0
calcula' z ((x,y):t) = x*(z)^y + calcula' z t

-- f)
simp :: Polinomio -> Polinomio 
simp [] = []
simp ((x,y):t)
 | y == 0 = simp t
 | otherwise = (x,y) : simp t

-- g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (x,y) ((a,b):t) = (x*a,y+b) : mult (x,y) t 

--h)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((x,y):t) = normalizaAux (x,y) (normaliza t)

normalizaAux :: Monomio -> Polinomio -> Polinomio
normalizaAux (a,b) [] = [(a,b)]
normalizaAux (a,b) ((x,y):t) 
 | b == y = (x + a, y) : t
 |otherwise = (x,y) : normalizaAux (a,b) t
  
soma :: Polinomio -> Polinomio -> Polinomio 
soma c [] = c 
soma [] a = a
soma ((x,y):xs) p =  normalizaAux (x,y) (soma xs p)

produto :: Polinomio -> Polinomio -> Polinomio
produto c [] = []
produto [] c = []
produto ((x,y):xs) p = (mult (x,y) p )++(produto xs p)

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((a,b):t) = ordenaAux (a,b) (ordena t)

ordenaAux :: Monomio -> Polinomio -> Polinomio
ordedenaAux c [] = [c]
ordenaAux (a,b) ((x,y):t)
 | y > b = (a,b): (x,y) : t
 -- | y == b && x > a = (a,b): (x,y) : t 
 -- | y == b && x < a = (x,y): (a,b) : t
 | otherwise = (x,y): ordedenaAux (a,b) t
 


 
 








    



 
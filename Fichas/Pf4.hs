module Pf4 where

import Data.Char

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (c:cs)
 | isAlpha c = (c : ll,ld) 
 | isDigit c = (ll,c : ld)
 | otherwise = (ll,ld)
  where (ll,ld) = digitAlpha cs

-- > nzp [0,2,3,-1,0,4]
-- (1,2,3)

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (n:ns)   
 | n < 0 = ( 1 + ne, z, p)  
 | n == 0 = (ne,1 + z,p)
 | n > 0 = (ne,z,1+p)
  where (ne,z,p) = nzp ns

-- > fromDigits [1,2,3,4]
-- 1234

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t

fromDigits' :: [Int] -> Int
fromDigits' l = fst (fromDigits'' l)
    where
      fromDigits''[] = (0,0)
      fromDigits'' (h:t) = let (r,e) = fromDigits'' t
                           in (h*10^e + r, e + 1)

fromDigitsTbIn :: [Int] -> Int
fromDigitsTbIn l = sumLce lce
    where lce = zip l (reverse [0..(length l - 1)])
          sumLce :: [(Int,Int)] -> Int
          sumLce [] = 0


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fib' n = snd (fib2 n)

fib2 :: Int -> (Int,Int)
fib2 0 = (0,0)
fib2 1 = (1,1)
fib2 n = let (r,va) = fib2 (n-1)
         in (r + va, r)

-- 9 b)
-- [(1,5),(2,4),(3,3),(4,2),(5,1)]

b9 = [(x,y)| x <- [1..5], y <- [1..5], x + y == 6]

b9' = [(x,6-x)| x <- [1..5]]

c9 = [[1..x]| x <- [1..5]]

--inits [1,2,3,4]
-- [[],[1],[1,2],[1,2,3],[1,2,3,4]]

inits l = [take n l | n <- [0..length l ]]


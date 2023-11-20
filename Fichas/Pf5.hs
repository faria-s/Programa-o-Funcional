module Pf5 where

import Data.List 

--any odd [1..10] == True

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) | f h = f  h 
            | otherwise = any' f t

any'' f [] = False
any'' f (h:t) = f h || any'' f t 

{- 1 : 2 : 3 : []
           /    |
           f     False
           =
           11 V -}

any''' f l = foldr aux False l 
     where aux a r = f a || r

--zipWith (+) [1,2,3,4,5] [10,20,30,40]  == [11,22,33,44]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (h:t) = f x h : zipWith' f xs t 
zipWith' _ _ _ = []

-- takeWhile odd [1,2,3,4,4,5,6,6] == [1,3]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f (h:t) 
 | f h = h : takeWhile' f t 
 | otherwise =  []

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f (h:t) 
 | f h = dropWhile' f t 
 | otherwise = h:t

span' :: (a-> Bool) -> [a] -> ([a],[a])
span' p l = (takeWhile' p l, dropWhile' p l)

span'' :: (a-> Bool) -> [a] -> ([a],[a])
span'' _ [] = ([],[])
span'' p (h:t) | p h = (h:a,b)
               | otherwise = ([], h:t)
                where (a,b) = span'' p t 

type Mat a = [[a]]
m :: Mat Int
m = [[1,2,3],[0,4,5],[0,0,6]]

dimOk' :: Mat a -> Bool
dimOk' [l] = True
dimOk' (h:t) = let larg = length h
              in dimOk' larg t
        where dimOk' _ [] = True
              dimOk' larg (l:ls)
                    | length l == larg = dimOk' larg ls
                    | otherwise = False

dimensoesLinhas :: Mat a -> Bool
dimensoesLinhas m = length (nub (map length m)) == 1

dimMat :: Mat a -> (Int,Int)
dimMat m = (length (head m), length m)

addMat :: Num a => Mat a -> Mat a -> Mat a 
addMat [] [] = []
addMat (h:t) (x:xs) = zipWith (+) h x :addMat t xs

an':: (a -> Bool) -> [a] -> Bool
an' f [] = False
an' f (h:t) 
 | f h == True = True
 |otherwise = an' f t 

zippWith :: (a->b->c) -> [a] -> [b] -> [c]
zippWith f c [] = []
zippWith f [] c = []
zippWith f (x:y) (h:t) = f x h : zippWith f y t

takkeWhile :: (a->Bool) -> [a] -> [a]
takkeWhile f (h:t) 
 | f h = h : takkeWhile f t 
 | otherwise = []

droppWhile :: (a->Bool) -> [a] -> [a]
droppWhile f (h:t)
 | f h = droppWhile f t 
 | otherwise = (h:t)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]



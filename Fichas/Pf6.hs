module Ficha6 where

data BTree a = Empty
               | Node a (BTree a) (BTree a) deriving Show

arv :: BTree Int
arv = Node 6 
        (Node 8 Empty
                    (Node 4 (Node 2 Empty Empty)
                                 Empty))
        (Node 5 (Node 9 Empty Empty)
                        Empty)                        

altura :: BTree a -> Int
altura Empty = 0
altura (Node _ l r) = 1 + max (altura l) (altura r)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ l r ) = 1 + contaNodos l + contaNodos r

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1 
folhas (Node _ l r) = contaNodos r + contaNodos l

prune :: Int -> BTree a -> BTree a
prune 0 a = Empty
prune n Empty = Empty
-- prune 1 (Node i l r) = Node i Empty Empty
prune n (Node i l r) = Node i (prune (n-1) r) (prune (n-1) r)

--False : esquerda
--True : direita
-- >>> path [False,True] arv
   --  [6,8,4]

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node i l r) = [i]
path (True:t) (Node i l r) = i : path t r
path (False:t) (Node i l r) = i : path t l

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node i l r) = Node i (mirror r) (mirror l)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node i l r ) (Node n e d) = Node (f i n) (zipWithBT f l e) (zipWithBT f r d)


unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (x,y,z) e d) = (Node x e1 d1, Node y e2 d2, Node z e3 d3)
                             where (e1,e2,e3) = unzipBT e 
                                   (d1,d2,d3) = unzipBT d

arvBP :: BTree Int
arvBP = Node 9 
        (Node 4 Empty
                    (Node 7 (Node 5 Empty Empty)
                                 Empty))
        (Node 12 (Node 11 Empty Empty)
                        Empty)     



minimo :: Ord a => BTree a -> a
minimo (Node i Empty _ ) = i 
minimo (Node _ e _ ) = minimo e 

semMinimo :: Ord a => BTree a -> BTree a 
semMinimo (Node i Empty d) = d
semMinimo (Node i e r ) = Node i (semMinimo e) r

remove :: Ord a => a -> BTree a -> BTree a 
remove e Empty = Empty
remove e (Node i l r )
 | e < i = Node i (remove e l) r
 | e > i = Node i l (remove e r)
 | e == i = case (l,r) of 
              (Empty,_) -> r 
              (_,Empty) -> l
              _ -> let m = minimo r 
                       r' = semMinimo r 
                    in Node m l r' 
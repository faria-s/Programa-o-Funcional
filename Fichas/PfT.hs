soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) = if x>0
               then x: soPos xs 
               else soPos xs

mytake :: Int -> [a] -> [a]
mytake n l | n <=0 = []
my take n [] = []
my take n (x:xs) = x : mytake (n-1) xs

mydrop :: Int -> [a] -> [a]
mydrop n l | n >= 0 = l
mydrop n [] = []
mydrop n (x:xs) = mydrop (n-1) xs

cola :: [a] -> [a] -> [a]
cola [] l2 = l2
cola l1 [] = l1
cola (x:xs) l2 = x : cola xs l2

inverte :: [a] -> [a]
inverte [] = []
inverte (x:xs) = (inverte xs) ++ [x]

fun :: [(a,b)] -> [b]
fun [] = []
fun ((a,b): xs) =  (fun xs) ++ [b]

myzip :: [a] -> [b] -> [(a,b)]
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys
myzip _ _ = []

somaPares :: [(Int,Int)] -> [Int]
somaPares ((x,y): t) = x+y : somaPares t
somaPares []= []

{-somas':: [(Int,Int)] -> (Int,Int)
somas' [] = (0,0)
somas' ((x,y):t) = (x+ fst (somas t), y+ snd(somas t) )

somas :: [(Int)] -> (Int,Int)

somas l = (somaFirst l, somaSecond l)
  where somaFirst :: [(Int,Int)] -> Int 
        somaFirst [] = 0
        somaFirst (h:t) = first h + somaFirst t
        somaSecond [] = 0
        somaSecond ((a,b):t) = b + somaSecond t 
        
A lista é pouco eficiente, pois corre duas vezes-}

somas :: [(Int,Int)] -> (Int,Int)
somas [] = (0,0)
somas ((x,y):t) = (x+sa,y+sb)
 where (sa,sb) = somas t

zip' :: [a]-> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

unzip' :: [(a,b)] -> ([a],[b])
unzip' []  = ([],[])
unzip' ((x,y): t) = (x:as,y:bs)
 where (as,bs) = unzip' t

{-splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n l = (take n l, drop n l)
splitAt' n l | n <= 0 = ([],l)
splitAt' _ [] = ([],[])
splitAt' n (x:xs) = (x:as, bs)
 where (as, bs) = splitAt' (n-1) xs 
 
 Funciona mas tem recomendações 

splitAt :: Int -> [a] -> ([a],[a])
splitAt n l = (take n l, drop n l)
splitAt n l | n <= 0 = ([],l)
splitAt _ [] = ([],[])
splitAt n (x:xs) = (x:l1, l2)
 where (l1,l2) = splitAt (n-1) xs -}

parte :: Ord a => a -> [a] -> ([a],[a])
parte _ []                 = ([],[])
parte x (y:ys) | y < x     = (y:as, bs)
               | otherwise = (as, y:bs)
    where (as,bs) = parte x ys

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (h:t) = (quickSort as) 
                  ++ [h] ++
                  (quickSort bs)
        where (as,bs) = parte h t



insere :: Ord a => a -> [a] -> [a]
insere x [] = [x]
insere x (h:t) | x < h = x:h:t 
               | otherwise = h : insere x t

insertionSort :: Ord a => [a] -> [a]
insertionSort [] =[]
insertionSort (x:xs) = insere x (insertionSort xs)

somatorio :: Num a => [a] -> a 
somatorio [] = 0
somatorio (h:t) = h + somatorio t


somatorio' :: Num a => [a] -> a 
somatorio' l = somatorioAc l 0 
 where somatorioAc [] ac = ac 
       somatorioAc (x:xs) ac = somatorioAc xs (x+ac)

maximo :: Ord a => [a] -> a 
maximo [x] = x
maximo (h:t) = max h (maximo t)

maximo' :: Ord a => [a] -> a 
maximo' (h:t) = maximoAc t h 
  where maximoAc :: Ord a => [a] -> a -> a 
        maximoAc [] ac = ac
        maximoAc (x:xs) ac | x > ac = maximoAc xs x
                           | otherwise = maximoAc xs ac 

inverte' :: [a] -> [a]
inverte' [] = []
inverte' l = inverteAc l []
  where inverteAc :: [a] -> [a] -> [a]
        inverteAc [] ac = ac 
        inverteAc (x:xs) ac = inverteAc xs (x:ac)



lc = [2*x | x <- [10,3,7,2]] -- não se usa e (pertence), mas sim "<-"

ex2 = [n| n <- [4,-5,8,20,-7,1], 0 <= n, n <= 10] -- não se usam &&, mas sim "," 

ex3 = [x| x <- [1..20], odd x]

ex4 = [(x,y) | x <- [1..4], y <- [10,11,20]]

ex4'= [(x,y) | x <- [10,11,20], y <- [1..4]]


parte' :: Ord a => a -> [a] -> ([a],[a])
parte' x l = ([v | v <- l, v < x], [v | v <- l, v >= x])

divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n], mod n x == 0]

primo :: Integer -> Bool
primo n = divisores n == [1,n]

primosAte :: Integer -> [Integer]
primosAte n = [x | x <- [1..n], primo x]

map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x) : map' f xs

-- map não altera o comprimento da lista, mas pode alterar o tipo

filter' :: (a-> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (h:t) 
 | p h = h : filter' p t 
 | otherwise = filter' p t 

-- dobro l = map (*2) l

-- \ função anónima Ex: map (\ x -> x/2) [1..20]

-- o filter fica com uma lista do mesmo tipo, msa altera tipicamente o tamanho

-- nº pares de uma lista de Int 

-- pares :: [Int] -> [Int]
-- pares l = filter even l 

-- Pre definida 
-- even :: Integral a => a -> Bool

-- >> even 3
-- > False
-- >> even 2
-- > True


-- (a -> b -> b) = (++) Ex: [7] = 7 : [] = 7 + 0
-- b = [] = 0 

foldr' ::(a -> b -> b) -> b -> [a] -> b  
foldr' f c [] = c 
foldr' f c (h:t) = f h  (foldr' f c t) 

somaPrimComp l = foldr' f 0 l 
   where f (num,nom) r = num + r 

somaPrimComp' l = foldr' (\ (num,nom) r -> num + r) 0 l -- uso da função anónima. Função sem nome, n é necessario usar o where (ATENÇÃO: em vez de = usa-se ->)


foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _  c  [] = c 
foldl' f c (h:t) = foldl' f (f c h) t

-- l' = map snd l 

-- l'' = foldr' (\ (n,no) r -> no:r) 

-- and = foldr (&&) tipicamente

-- product l =  foldr (*) 1

-- concat :: [[a]] -> [a]
-- concat l = flodr (++) []

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

-- >> :t Node 
-- > Node :: a -> (BTree a) -> (BTree a)

arv :: BTree Int
arv = Node 3 
           (Node 4
                 (Empty)
                 (Node 7 Empty Empty))
            (Node 5 Empty Empty) 

{-         3
          / \
         /   \
        4     5
       / \    /\
      x   7   x x
          /\
         x  x     -}

conta :: BTree a -> Int
conta Empty = 0
conta (Node i l r) = 1 + conta l + conta r 

-- sumBTree arv = 19

sumBTree :: Num a => BTree a -> a
sumBTree Empty = 0 
sumBTree (Node valor left right) = valor + sumBTree left  + sumBTree right

altura :: BTree a -> Int 
altura Empty = 0
altura (Node _ l r ) 
 | altura l > altura r = 1 + altura l 
 | otherwise = 1 + altura r

altura' Empty = 0
altura' (Node _ e d) = 1 + max (altura' e) (altura' d) 

-- Notação prefixa -> (+) 2 3 = 5

-- >> prefixa arv
-- > [3,4,7,5] pai -> esquerda -> direita

-- >> Infixa arv
-- > [4,7,3,5]  esquerda -> pai -> direita 

-- >> posfixa arv
-- > [7,4,5,3] esquerda -> direita -> pai

prefixa :: BTree a -> [a] 
prefixa Empty = []
prefixa (Node i e d ) = 
       i : prefixa e ++ prefixa d

posfixa :: BTree a -> [a] 
posfixa Empty = []
posfixa (Node i e d ) = 
      posfixa e ++ posfixa d ++ [i]

mapBT :: (a -> b) -> BTree a -> BTree b
mapBT _ Empty = Empty
mapBT f ( Node i e d ) = 
            Node (f i) (mapBT f e ) (mapBT f d )

zipBTree :: BTree a -> BTree b -> BTree (a,b)
zipBTree (Node i e d) (Node i2 e2 d2)
        = Node (i,i2) (zipBTree e e2)
                      (zipBTree d d2)

zipBTree _ _ = Empty

elemBT :: Ord a => a -> BTree a -> Bool
elemBT el Empty = False
elemBT el (Node i e d) | el == i = True
                       | el > i = elemBT el d 
                       | el > i = elemBT el e 

insertBT :: Ord a => a -> BTree a -> BTree a 
insertBT e Empty 
 | e == i = Node i l r 
 | e < i = Node i (insertBT e l) d 
 | e > i = Node i e (insertBT e r)

balanceamento :: BTree a -> BTree a 
balanceamento t = constroi (infixa t)

constroi :: [a] -> BTree a 
constroi [] = Empty 
constroi l = let c = length l 
               (le,ld) = splitAt (div c 2) l 
             in Node (head ld)
                     (constroi le)
                     (constroi (tail ld))






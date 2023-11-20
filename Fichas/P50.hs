
--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y 
 |x > y = []
 |otherwise = x: enumFromTo' (x+1) y

--2 
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x c y 
 | x > y = []
 |otherwise = x : enumFromThenTo' (x+c-1) c y

--3
mais :: [a] -> [a] -> [a]
mais [] l = l
mais l [] = l
mais (l:c) t = l : mais c t

--4
ex :: [a] -> Int -> a
ex (h:t) 0 = h 
ex (h:t) n = ex t (n-1)

--5 
reverse' :: [a] -> [a]
reverse' [] = []
reverse' l = last l : reverse' (init l)

--6
take' :: Int -> [a] -> [a]
take' 0 l = []
take' i (h:t) = h : take' (i-1) t

--7
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (h:t)
 | n <= 0 = (h:t)
 | otherwise = drop' (n-1) t

--8
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h:t) (x:y) = (h,x) : zip' t y

--9
replicate' :: Int -> a -> [a]
replicate' 0 c = []
replicate' n c = c : replicate' (n-1) c

--10
intersperse' :: a -> [a] -> [a]
intersperse' n [] = []
intersperse' _ [x] = [x]
intersperse' n (h:t) = h : n : intersperse' n t

--11
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [x] = [[x]]
group' (h:t) 
 | elem h (head r) = (h : (head r)) : tail r 
 | otherwise = [h] : r
 where r = group' t

--12
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

--13
inits' :: [a] -> [[a]]
inits' [] =  [[]]
inits' l = inits' (init l) ++ [l]

--14
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = [l] ++ tails' (tail l)

--15
heads' :: [[a]] -> [a]
heads' [] = []
heads' ([]:t) = heads' t
heads' (l:t)  = head l : heads' t

--16
total :: [[a]] -> Int
total [] = 0
total (l:t) = length l + total t

--17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = (a,c) : fun t

--18
cola :: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):t) = a ++ cola t

--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade a i ((x,y):t) 
 | a-y >= i = x : idade a i t 
 | otherwise = idade a i t

--20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [1]
powerEnumFrom n m 
 | m > 1 = powerEnumFrom n (m-1) ++ [n^m]
 |otherwise = []

--21
isPrime :: Int -> Bool
isPrime n = n >= 2 && primeCheck n 2

primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m * m > n = True
    | mod n m == 0 = False
    | otherwise = primeCheck n (m + 1)

--22
isPrefixOf :: Eq a => [a]-> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:y) (h:t)
 | x ==  h && isPrefixOf y t = True
 | otherwise = False 

--23





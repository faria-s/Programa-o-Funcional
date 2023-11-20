
module Pf2 where

import Data.Char

type Hora =  (Double, Double)

dist :: Hora -> Hora -> Double
dist (x,y) (m,s) = sqrt((x-m)^2 + (y-s)^2)

valida :: Hora -> Bool
valida (x,y) = if ((0<=x) && (x<24) && (0<=y) && (y<60)) 
             then True 
              else False

depois :: Hora -> Hora -> Bool
depois (y,m) (x,s) = if (x>y) then True
                       else if ((x == y) && (s>m)) then True
                        else False
                            

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)
next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

stop :: Semaforo -> Bool
stop Vermelho = True
stop x = False

safe :: Semaforo -> Semaforo -> Bool
safe Vermelho x = True
safe x Vermelho = True
safe _ _ = False

data Ponto = Cartesiano Double Double | Polar Double Double  deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar d a ) = d * (cos a)

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar d a ) = d * (sin a)

raio :: Ponto -> Double
raio (Cartesiano x y )= sqrt(x^2 + y^2)
raio (Polar d c ) = d

angulo :: Ponto -> Double
angulo (Polar d c) = c
angulo (Cartesiano x y) 
        | y>=0 = atan(y/x)
        | otherwise = pi + atan(y/x)

                                      
comprimento :: Ponto -> Ponto -> Double
comprimento p1 p2 = (dist (posx p1, posy p1) (posx p2, posy p2))

data Figura = Circulo Hora Double | Rectangulo Hora Hora | Triangulo Hora Hora Hora deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Circulo (x,y) m) = False
poligono (Rectangulo (x,y) (m,s)) = if (x,y) == (m,s) then False else True
poligono (Triangulo (x,y)(m,s)(d,c)) = if (x*s + y*d + m*c - d*s - c*x - m*y) == 0
                                        then False
                                          else True

data Maybe a = Nothing | Just a

vertices :: Figura -> [Hora] 
vertices (Rectangulo (x,y) (m,s)) = [(x,y), (m,s), (m,y), (x,s)]
vertices (Triangulo (x,y) (m,s) (d,c)) = [(x,y), (m,s), (d,c)]
vertices (Circulo(c1,c2)r) = error "Não tem vértices"

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
 let a = dist p1 p2
     b = dist p2 p3
     c = dist p3 p1
     s = (a+b+c) / 2 -- semi-perimetro
 in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron

area (Rectangulo (x,y) (m,s)) =
 let a = (s-y)
     b = (m-x)
 in a*b

area (Circulo p1 r) = r^2*pi

perimetro :: Figura -> Double
perimetro (Rectangulo (x,y) (m,s)) =
 let a = (s-y)
     b = (m-x)
 in 2*a * 2*b

perimetro (Triangulo p1 p2 p3) =
 let a = dist p1 p2
     b = dist p2 p3
     c = dist p3 p1
 in a+b+c

perimetro (Circulo p1 r) = 2*r*pi



isLower' :: Char -> Bool
isLower' ch = ch `elem` ['a'..'z']

isDigit' :: Char -> Bool
isDigit' ch = ch `elem` ['0'..'9']

isAlpha' :: Char -> Bool
isAlpha' ch = (ord ch >= ord 'A' && ord ch <= ord 'z')

toUpper' :: Char -> Char
toUpper' ch
  | isLower' ch = chr (ord ch - (ord 'a' - ord 'A'))
  | otherwise = ch

intToDigit' :: Int -> Char
intToDigit' n
    | n >= 0 && n <= 9 = chr (ord '0' + n)
    | otherwise        = error "O número deve estar entre 0 e 9"


module Pf3 where

import Data.Char


data Hora = H Int Int deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

v :: Viagem
v = [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]

vInvalida :: Viagem
vInvalida = [(H 9 30, H 10 25), (H 10 20, H 12 45), (H 13 30, H 14 45)]


horaValida :: Hora -> Bool
horaValida (H x y) = ((0<=x) && (x<24) && (0<=y) && (y<60)) 
             

comparacaohora :: Hora ->  Hora -> Bool
comparacaohora (H h1 m1) (H h2 m2) = if h1  > h2 then True
                                     else if h1 >= h2 && m1 > m2 then True
                                    else False

etapaValida :: Etapa -> Bool
etapaValida (hi,hf) = (horaValida hi && horaValida hf && comparacaohora hf hi)


viagemValida :: Viagem -> Bool
viagemValida [] = False
viagemValida [e] = etapaValida e
viagemValida (e1:e2:ef) = comparaEtapa e1 e2 && viagemValida (e2:ef)

comparaEtapa :: Etapa -> Etapa -> Bool
comparaEtapa e1 e2 = comparacaohora (fst e2) (snd e1)

horaPartidaChegada :: Viagem -> (Hora,Hora)
horaPartidaChegada v = (fst(head v), snd(last v))

etapaEmMinutos :: Etapa -> Int
etapaEmMinutos (h1,h2) = diffHoras h1 h2 


diffHoras :: Hora -> Hora -> Int 
diffHoras (H h1 m1) (H h2 m2) = 
           abs ((h1-h2)*60 + (m1-m2))


tempoTotalViagem ::Viagem -> Int
tempoTotalViagem [] = 0
tempoTotalViagem (x:xs) = etapaEmMinutos x + tempoTotalViagem xs

tempoEmEspera :: Viagem -> Int
tempoEmEspera v = etapaEmMinutos (horaPartidaChegada v) - tempoTotalViagem v

tempoTotalViagem' :: Viagem -> Int
tempoTotalViagem' v = 
    let (hi,hf) = horaPartidaChegada v 
    in etapaEmMinutos (hi,hf)

type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String

data Data = D Dia Mes Ano deriving Show

type TabDN = [(Nome,Data)]

amigos :: TabDN
amigos = [("Zé" , D 10 10 2010), ("Ana" , D 5 5 2005), ("Rui" , D 1 1 2001) ]

procura :: Nome -> TabDN -> Maybe Data
procura n [] = Nothing 
procura n ((no,dn):as)
          | n == no = Just dn
          | otherwise = procura n as 

idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade d n (...)  











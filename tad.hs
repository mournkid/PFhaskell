import Data.Char 

import Data.List



numUnicos :: (Eq a) => [a] -> Int
numUnicos xs = (length.nub) xs

ocorre :: String -> String -> Int 
ocorre palavra texto = snd (tupla iguais)
    where lista = words texto 
          ordenada = sort lista 
          iguais = filter (\x -> x == palavra) ordenada
          tupla :: [String] -> (String,Int)
          tupla xs = (palavra,length iguais)

prefixo :: String -> [String] -> Bool 
prefixo x xs = or (map (\y-> isPrefixOf x y ) xs)
  
-- construção de modulos 

contida :: [Int] -> [Int] -> Bool 
contida xs ys = xs `isInfixOf` ys 

cifracao :: Int -> String -> String
cifracao x xs = map aux xs 
  where aux y = chr ((ord y) + x)


valorDoMeio :: Int -> Int -> Int 
valorDoMeio x y z 
  | entre y x z = y 
  | entre x y z = x 
  | otherwise = z 
    where entre a b c = (a<=b) && (a<=c) || (a>=b) && (a>=c)

{-valorDoMeio (7+50) (max 10 30) (10*4) 
 ?? entre (max 10 30) (7+50) (10*4) 
 ?? ((max 10 30) <= (7+50)) && ( (7+50) <= (10*4))
     || ((max 10 30) >= (7+50)) && ( (7+50) >= (10*4)) 
 ?? ( 30 <= 57) && ( 57 <= (10*4))
     || (30 >= 57) && ( 57>= (10*4)) 
 ?? True && ( 57 <= (10*4))
     || (30 >= 57) && ( 57>= (10*4)) 
 ?? True && ( 57 <= 40)
     || (30 >= 57) && ( 57>= 40) 
 ?? True && False     || (30 >= 57) && ( 57>= 40) 
 ?? False || (30 >= 57) && ( 57>= 40) 
 ?? False || ( True) && ( 30 >= 40) 
 ?? False || ( True) && ( False)
 ?? False || False
 ?? False 
 ?? otherwise
 ?? 40-}


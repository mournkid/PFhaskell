import Data.Char 

bissexto :: Int -> Bool 
bissexto x 
  |mod x 4 == 0 && mod x 100 == 0 && mod x 400 /= 0 = False
  |mod x 4 == 0 = True 
  |otherwise = False


proporcao :: String -> Float 
proporcao xs = fromIntegral (oc "aA" xs)/ fromIntegral (oc "aAeEiIoOuU" xs) 
     where oc xs ys = length [ y | y<-ys, elem y xs]

{-Um par cartesiano (a,b) é ordenado se a ≤ b. Suponha uma lista xs de inteiros
positivos, contendo a mesma quantidade de elementos pares e ímpares. Elabore a
função duplaImparParOrd, tal que dado xs, devolve uma lista de pares
cartesianos formados a partir das listas de pares e ímpares da lista original, de tal forma
que o par cartesiano é ordenado e sempre possuirá um elemento par e outro ímpar.
Todos os elementos da lista original aparecem na lista de pares cartesianos. Por
exemplo, para
duplaImparParOrd [1,2,5,4] a função devolverá [(1,2),(4,5)]
duplaImparParOrd [1,2,5,2] a função devolverá [(1,2),(2,5)].-}

duplaImparParOrd :: [Int] -> [(Int,Int)]
duplaImparParOrd xs = [ordem x | x<-lista]
  where impar = [x | x<-xs, odd x]
        par = [x | x<-xs, even x]
        ordem (a,b)
          | a<=b = (a,b)
          |otherwise = (b,a)
        lista = zip impar par 

somar :: [Int] -> Int 
somar xs = last ys
   where ys = 0 : map (\(a,b)-> a+b) (zip xs ys) 

nAnd :: Bool -> Bool -> Bool 
nAnd True True = False
nAnd _ _ = True 

media :: Float -> Float -> Float -> Float -> Float -> Float 
media a b c d e = (((a+b)/2)*2 + ((c+d)/2)*2 + e*6)/10

menor :: Float -> Float -> Float -> Float
menor x y z = if x < y && y < z then x else if y < x && x < z then y else z
  

{-a :: Float -> Float -> Float -> Float
a x y z = let a1 = x*y a2 = x*z a3 = y*z 
          in 2*(a1+a2+a3)-}


mediaList :: [Float] -> Float
mediaList list = somaElementos /  tamanhoLista
  where tamanhoLista = fromIntegral (length list)
        somaElementos = fromIntegral (floor(sum list)) 

adjetivos :: [String] -> [String] -> [String]
adjetivos xs ys = [x ++ " " ++ y | x<-xs,y<-ys]

import CodeWorld

main = drawingOf xadrez

--tabuleiro = pictures [(translated j i (coloured black (solidRectangle 1 1))) & (translated i j (coloured white (solidRectangle 1 1))) | j<-[1,3..8], i<-[2,4..8]] 
  
  
xadrez = pictures [ (translated 0 i fila1) &  (translated 0 j fila2)  | i<-[1,3..7], j<-[2,4..8]]
   where fila1 = pictures [translated i 0 (colored black (solidRectangle 1 1)) & translated j 0 (colored white (solidRectangle 1 1)) | i<-[1,3..7],j<-[2,4..8]]
         fila2 = pictures [translated i 0 (colored white (solidRectangle 1 1)) & translated j 0 (colored black (solidRectangle 1 1)) | i<-[1,3..7],j<-[2,4..8]]


























{-circulos :: Double -> Picture 
circulos x = pictures [ translated ((cos i)*2) ((sin i)*2)  (circle 2) | i <- [0,y..2*pi]] 
  where y = (2*pi)/x-}






{-quadrados :: Double -> Picture 
quadrados x = pictures [rotated (i*pi/9) (rectangle i i) | i<-[1..x]] -}















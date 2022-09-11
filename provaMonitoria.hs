import Data.Char

--4)
h :: String -> Int 
h xs = sum (pos indexa)
  where indexa = zip xs [0..]
        pos :: [(Char,Int)] -> [Int]
        pos [] = [] 
        pos ((a,b):xs)
          | a>= 'A' && a<='Z' = b : pos xs
          |otherwise = pos xs

--2)
-- c :: ((t1,t2) -> t3) -> t1 -> t2 -> t3
-- uc :: (t1 -> t2 -> t3) -> (t1,t2) -> t3

--3) 

eulerA :: Int -> Float
eulerA n = sum [i x | x<-[0..n]]
    where fatorial x =  fromIntegral (product [1..x])
          i x 
            |x == 0 = 1
            |otherwise = 1 / fatorial x 

eulerB :: Int -> Float
eulerB n = foldr (+) 0 (map i [0..n])
  where fatorial x = fromIntegral (foldr (*) 1 [1..x])
        i x = 1 / fatorial x
          
eulerC :: Int -> Float
eulerC 0 = 1
eulerC n = (1 / fromIntegral(fatorial n)) + eulerC (n-1) 
   where fatorial 0 = 1
         fatorial x = fatorial (x-1) * x

serie :: Int -> (Int, Int)
serie 1 = (0, 1)
serie 2 = (1, 2)
serie 3 = (2, 3)
serie 4 = (3, 6)
serie 5 = (6, 11)
serie n = (p, q)
    where p = snd (serie (n-1))
          q = snd (serie (n-1)) + snd (serie (n-2)) + snd (serie (n-3)) 



-- 1)

{- q1 [1,3,4,2] -> max (q1 [4,2]) 3 -> max (max (q1 []) 2) 3 -> max (max 0 2) 3 -> max 2 3 -> 3-}

{-
funcao :: [(Int,Int)] -> [((Int,Int),(Int,Int))]
funcao xs = [(posicao xs x, posicao xs (auxiliar (x+1))) | x<-lista]
  where auxiliar :: Int -> Int 
        auxiliar c 
          | c == ((last lista) + 1) = 1 
          | otherwise = c
        posicao :: [(Int,Int)] -> Int -> (Int,Int)
        posicao ys y = head [ a | (a,b) <-(zip ys [1..length ys]), b == y] 
        lista = [1..length xs]


posicao1 :: [(Int,Int)] -> Int -> (Int,Int)
posicao1 xs x = head [ a | (a,b) <- (zip xs [1..length xs]), b == x]
 ((x,y),z)

 [(2,3),(3,4),(5,6)]

 -> [((2,3),(3,4)),((3,4),(5,6)),((5,6),(2,3))]-}


 somatoria :: 
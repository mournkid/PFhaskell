import Data.Char

somaNaoMaior :: Int -> Int -> Int -> Int
somaNaoMaior x y z 
  |x /= y && x/=z && y/=z = x + y + z - (max x y z) 
  |x/=y && x==z || x==y && x/=z || x/=y && y==z = x + y + z -((max x y z)*2)
  |otherwise = 0
    where max :: Int -> Int -> Int -> Int
          max x y z
           | x >= y && x >= z = x
           | y >= x && y >= z = y
           | otherwise = z  

semOrdem :: [Int]->Bool
semOrdem xs 
  |[x | x<-xs, mod x 2 == 0] ++ [y  | y <-xs, mod y 2 /= 0 ] /= []  = False 
  |[y | y <-xs, mod y 2 /= 0] ++ [ x | x<-xs, mod x 2 == 0] /= []  = False
  |otherwise = True  

duplaImparParOrd :: [Int]->[(Int,Int)]
duplaImparParOrd xs = zip [a | a<-xs, mod a 2==0] [b | b<-xs, mod b 2 /=0]

                     
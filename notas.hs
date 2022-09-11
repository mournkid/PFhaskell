




  --Escreva uma função para atualizar o segundo elemento do i-ésimo par
--de uma lista de pares, com o produto do primeiro valor do par pelo
--segundo valor.

indexaLista :: [(Int,Int)] -> [(Int, (Int,Int))]
indexaLista xs = zip ls xs
      where ls = [1..(length xs)]

encontraPosElem :: (Int,Int)->[(Int, (Int,Int))]->Int
encontraPosElem (x,y) xys
  | ps == [] = 0
  | otherwise = head ps
     where ps = [p | (p, (u,v))<-xys, (u==x && v==y)]   

atualizarSnd :: [(Int,Int)]->(Int,Int)->[(Int,Int)]
atualizarSnd xs (x,y) = inicio ++ [termo] ++ final
  where posicao = encontraPosElem (x,y) (indexaLista xs)
        inicio = take (posicao-1) xs
        termo  = atualiza (xs !! (posicao-1))
        final  = drop posicao xs
        atualiza :: (Int,Int)->(Int,Int)
        atualiza (a,b) = (a,a*b) 

--Escreva uma função para atualizar uma tupla (x,y,z) de uma lista de
--tuplas de três elementos inteiros, com uma tupla da forma (x, x+y,
--x+y+z).
atualizar3 :: [(Int,Int,Int)] -> [(Int,Int,Int)]
atualizar3 xs = [(a,b+c,a+b+c) | (a,b,c)<-xs]
 
somaNaoMaior :: Int -> Int -> Int -> Int
somaNaoMaior x y z 
  | x==y && x==z = 0 
  | x==y && x/=z || x==z && x/=y || y==z && y/=x = (x + y + z) - 2*(max x (max y z)) 
  |otherwise = (x + y + z) - (max x (max y z))

somaLI :: [[Int]] -> Int
somaLI xs 
  |xs == [] = error "lista invalida"
  |otherwise = sum [ sum  x | x<-xs, (head x) == 1 || (head x) == 100] 

semOrdem :: [Int] -> Bool
semOrdem xs 
  |xs == [x | x<-xs, mod x 2 /= 0] || xs == [x | x<-xs, mod x 2 == 0] = error "lista invalida"
  |xs == [x | x<-xs, mod x 2 /= 0] ++ [x | x<-xs, mod x 2 == 0] = False
  |xs == [x | x<-xs, mod x 2 == 0] ++ [x | x<-xs, mod x 2 /= 0] = False
  |otherwise = True 

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
duplaImparParOrd xs 
  |lista == True = [ord x | x<-indexa xs]
  |otherwise = error "lista invalida"
    where lista = length [x | x<- xs, mod x 2 ==0] == length [y | y<- xs, mod y 2 /= 0]
          indexa :: [Int] -> [(Int,Int)] 
          indexa xs = zip [x | x<- xs, mod x 2 ==0] [y | y<- xs, mod y 2 /= 0]
          ord :: (Int,Int) -> (Int,Int)
          ord (a,b) 
            |a<=b = (a,b)
            |otherwise = (b,a)


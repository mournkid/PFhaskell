--unzip :: [(a,b)] -> ([a],[b])
--splitAt :: Int -> [a] -> ([a],[a])
--reverse :: [a] -> [a]
--concat :: Foldable t => t [a] -> [a]
--replicate :: Int -> a -> [a]
-- (:) :: a -> [a] -> [a]
-- (!!) :: [a] -> Int -> a
-- (++) :: [a] -> [a] -> [a]
--odd :: Int -> Bool
--isDigit :: Char-> Bool

import Data.Char

roundAll :: [Float] -> [Int]
roundAll [ ] = [ ]
roundAll (y:ys) = round y : roundAll ys

maiusculo :: String -> String
maiusculo [ ] = [ ]
maiusculo (c:cs) = toUpper c : maiusculo cs

maiusculo1 :: String -> String
maiusculo1 cs = map toUpper cs

dobroLista :: [Int] -> [Int]
dobroLista xs = map dobro xs
    where dobro x = 2*x

ordString :: String -> [Int]
ordString xs = map ord xs 

quadrado :: [Int] -> [Int]
quadrado xs = map sqr xs 
  where sqr x = x^2

seconds :: [(Int,Int)] -> [Int]
seconds xs = map segundo xs
    where segundo (a,b) = snd (a,b) 

length1 :: [Int] -> Int
length1 xs = sum (funcao xs) 
 where funcao :: [Int] -> [Int]
       funcao xs = map um xs 
         where um x = 1 

dobro :: [Int] -> [Int]
dobro xs = zipWith (*) xs (replicate (length xs) 2)

quadrado1 :: [Int] -> [Int]
quadrado1 xs = zipWith (^) xs (replicate (length xs) 2)

digitos :: String -> Bool
digitos xs = and (map digito xs) 
     where digito x 
             |x >= '0' && x <= '9' = False 
             |otherwise = True 

letras :: String -> Bool
letras xs = or (map naoLetra xs)
  where naoLetra x 
          | x >= 'a' && x <= 'z' = False
          | x >= 'A' && x <= 'Z' = False
          | otherwise = True

impares :: [Int] -> [Int]
impares xs = [x | x <- xs, odd x]

impares1 :: [Int] -> [Int]
impares1 xs = filter odd xs

digitos1 :: String -> String
digitos1 cs = [c | c <- cs, isDigit c]

digitos2 :: String -> String
digitos2 cs = filter isDigit cs

minusculas :: String -> String
minusculas cs = [c | c<-cs, ehMinusculo c]
  where ehMinusculo c = ('a' <= c) && (c <= 'z')

minusculas1 :: String -> String
minusculas1 cs = filter ehMinusculo cs
   where ehMinusculo c = ('a' <= c) && (c <= 'z')

positivos :: [Int] -> [Int]
positivos xs = filter positivo xs 
   where positivo x = x > 0 

paresc :: [(Int,Int)] -> [(Int,Int)]
paresc xs = filter filtrar xs 
  where filtrar (a,b) = (a,b) == (a,a^2)

listaOrdenada :: [[Int]] -> [[Int]]
listaOrdenada xs = filter (/=[]) (map auxiliar xs)
  where auxiliar :: [Int] -> [Int]
        auxiliar xs 
          |xs == quickSort xs = xs 
          |otherwise = []

{-quickSort :: [Int] -> [Int]
quickSort [] = [] 
quickSort (x:xs) = quickSort (us xs x)  ++ [x] ++ quickSort (vs xs x ) 
     where us :: [Int] -> Int -> [Int]
           us [] _ = []
           us (y:ys) x 
             |y <= x = y : us ys x 
             |otherwise = us ys x 
           vs :: [Int] -> Int -> [Int]
           vs [] _ = []
           vs (y:ys) x 
             |y > x = y : vs ys x 
             |otherwise = vs ys x-}
ordInsercao :: [Int] -> [Int]
ordInsercao xs = foldr insOrd [] xs
  where insOrd :: Int -> [Int] -> [Int]
        insOrd z xs = takeWhile menorz xs ++ [z] ++ dropWhile menorz xs
          where menorz y = y <= z

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = foldr aux [] (x:xs)
  where aux :: Int -> [Int] -> [Int]
        aux x xs = filter menor xs ++ [x] ++ filter maior xs 
          where menor y = y <= x 
                maior y = y > x 
        

posicao :: String -> Char -> Int 
posicao xs x 
  |(filter (==x) (xs)) == [] = -1 
  |otherwise = snd (last (pos indexa x))  
   where indexa = zip xs [1..]
         pos :: [(Char,Int)] -> Char -> [(Char,Int)]
         pos xs x = filter aux xs 
           where aux (a,b) = a == x 

dobroImpar :: [Int] -> [Int]
dobroImpar xs = zipWith (*) (aux xs) (replicate (length (aux xs)) (2))
  where aux xs = filter odd xs

{-Função para dada uma propriedade p e uma lista, remove da lista de entrada
o primeiro elemento que não satisfaz p. Para testar a função use algumas
propriedades diferentes.-}

removefst :: [Int] -> [Int]
removefst xs = filter (/= (auxiliar xs)) xs
  where auxiliar xs = head (filter odd xs)  

removefst1 :: [Int] -> [Int]
removefst1 xs = filter (/= (auxiliar xs)) xs
  where auxiliar xs = head (filter even xs)


{-Função para dada uma propriedade p e uma lista, remove da lista de entrada
o último elemento que não satisfaz p.-}

removelst :: [Int] -> [Int]
removelst xs = filter (/= (auxiliar xs)) xs
  where auxiliar xs = last (filter odd xs)

{-Função para selecionar os elementos que ocupem posições pares na lista de
entrada.-}

posPar :: [Int] -> [Int]
posPar xs = map fst (pos indexa)
  where indexa = zip xs [1..]
        pos :: [(Int,Int)] -> [(Int,Int)]
        pos xs = filter par xs 
        par (a,b) = even b

posImpar :: [Int] -> [Int]
posImpar xs = map fst (pos indexa)
  where indexa = zip xs [1..]
        pos :: [(Int,Int)] -> [(Int,Int)]
        pos xs = filter impar xs 
        impar (a,b) = odd b 

{-Função para dada uma lista gerar um par onde o primeiro elemento do par
contém a lista de elementos das posições pares e o segundo elemento do par
a lista dos elementos das posições ímpares.-}
  

posicoes :: [Int] -> ([Int],[Int])
posicoes xs = (posPar xs, posImpar xs)

mySum :: [Int] -> Int
mySum zs = foldr (+) 0 zs

and1 :: [Bool] -> Bool
and1 zs = foldr (&&) True zs

concat1 :: [[Int]] -> [Int]
concat1 zss = foldr (++) [] zss

maior :: [Int] -> Int
maior zs = foldr1 max zs

reverse1 :: [Int] -> [Int]
reverse1 xs = foldr snoc [] xs
  where snoc x xs = xs ++ [x]   

length2 :: [a]-> Int
length2 xs = foldr g 0 xs
  where g :: a -> Int -> Int
        g _ n = n+1   

myOr :: [Bool] -> Bool
myOr [] = False
myOr xs = foldr1 (||) xs 

myProduct :: [Float] -> Float
myProduct xs = foldr (*) 1 xs 

fatorial :: Int -> Int
fatorial x = foldr (*) 1 [1..x]

menor :: [Int] -> Int 
menor xs = foldr1 min xs 











{-Função para calcular o menor par de uma lista de pares de
inteiros. Dados dois pares (a,b) e (c,d). O par (a,b) é menor
que o (c,d) se:

a < c ou

a =c e b≤d.-}

menorPar :: [(Int,Int)] -> (Int,Int)
menorPar xs = foldr1 menor xs 
  where menor :: (Int,Int) -> (Int,Int) -> (Int,Int)
        menor (a,b) (c,d)
          | a < c = (a,b)
          | a == c && b <= d = (a,b)
          | otherwise = (c,d)


somaImpares :: [Int] -> Int 
somaImpares xs = foldr1 (+) (impares xs)
  where impares :: [Int] -> [Int]
        impares xs = filter odd xs 

{-Função para realizar o produto dos quadrados de uma lista de inteiros.-}

prodQuad :: [Int] -> Int
prodQuad xs = foldr1 (*) (a xs)
  where a :: [Int] -> [Int]
        a xs = map (^2) xs

{-Função para calcular o produto dos quadrados dos números maiores que 3 de uma lista de inteiros.​-}

produto3 :: [Int] -> Int
produto3 xs = foldr1 (*) (quadrado3 (maior3 xs))
  where maior3 :: [Int] -> [Int]
        maior3 xs = filter maior xs 
        maior x = x > 3
        quadrado3 :: [Int] -> [Int]
        quadrado3 xs = map (^2) xs 

{-Função para retornar uma string, toda ela com caracteres maiúsculos, formada da concatenação de palavras de uma lista de palavras.​-}
  
upperConcat :: [String] -> String 
upperConcat xs = map toUpper (c xs) 
  where c :: [String] -> String
        c xs = foldr1 (++) xs 

{-Função para retornar uma string resultante da concatenação dos dígitos existentes em palavras de uma lista de palavras. 
Ex: [“Rua A,  12”, “Rua B, 16”] retornará “1216”.​-}

numeros :: [String] -> String 
numeros xs = filter isDigit (n xs) 

n :: [String] -> String
n xs = foldr1 (++) xs


funcaoAux :: (a -> b) -> (a -> b) -> (a,Int) -> b
funcaoAux f g (x,y)
 |mod y 2 /= 0 = f x
 |otherwise = g x

funcao :: (a -> b) -> (a -> b) -> [a] -> [b]
funcao f g xs = map (funcaoAux f g) (zip xs [1.. length xs])

switchMap :: [Int] -> [Int]
switchMap xs = funcao (+1) (+10) xs 


primPalavra :: String -> String
primPalavra xs = takeWhile letra xs 
  where letra x = x /= ' ' 

descPrimPal :: String -> String 
descPrimPal xs = dropWhile letra xs
  where letra x = x /= ' '  

pulaDemarcadores :: String -> String
pulaDemarcadores xs = filter c xs 
  where c x = x /= '\t' && x /= '\n' && x /= ' '

listaPalavras :: String -> [String]
listaPalavras [] = []
listaPalavras [x] = [[x]]
listaPalavras (x:xs) 
  | x == ' ' = listaPalavras xs
  | otherwise = primPalavra (x:xs) : listaPalavras (descPrimPal xs)

-- GENERALIZAÇÃO

indexa :: String -> [(Char, Int)]
indexa str = zip str [1..]

filtrarMaiusculas :: [(Char, Int)] -> [(Char,Int)]
filtrarMaiusculas ps = filter maiusc ps
  where maiusc (c, p) = isUpper c

segundos :: [(Char, Int)] -> [Int]
segundos ps = map snd ps

somaPosMaiusc :: String -> Int
somaPosMaiusc str = sum (segundos (filtrarMaiusculas (indexa str)))

filtrarMaiusculas1 :: [(Char, Int)] -> [(Char,Int)]
filtrarMaiusculas1 ps = filter maiusc ps
  where maiusc = isUpper . fst

somaPosMaiusc1 :: String -> Int
somaPosMaiusc1 str = (sum . segundos . filtrarMaiusculas1) (indexa str)

pulaNaoDigito :: String -> String
pulaNaoDigito xs = dropWhile (not . isDigit) xs

iter :: Int -> (a -> a) -> (a -> a)
iter x f = f . iter (x-1) (f)

twice :: (a->a) -> (a->a)
twice f = f . f

pot2 :: Int -> Int 
pot2 x = iter x dobro 2  
  where dobro x = 2*x 

{-($) :: (a -> b) -> a -> b-}

adUmTodos xs = map (\n -> n+1) xs

f :: Char -> Bool 
f x = not $ and (map (\x -> elem x " \n\t") [x])

mult :: Int -> Int -> Int
mult x y = x*y

dobroLista1 :: [Int] -> [Int]
dobroLista1 xs = map (mult 2) xs

pares :: [Int] -> [Int]
pares xs = filter (\x -> mod x 2 == 0) xs 

maxTres :: Int -> Int -> Int -> Int
maxTres m n p = max m (max n p)

max1 = maxTres 10 5 


quickSort1 :: [(Int,String)] -> [(Int,String)]
quickSort1 xs = foldr aux [] xs
  where aux :: (Int,String) -> [(Int,String)] -> [(Int,String)]
        aux (a,b) xs = filter ordPar xs ++ [(a,b)] ++ filter ordPar1 xs
          where ordPar (n1,w1) = w1 < b || (w1==b && n1 < a)
                ordPar1 (n1,w1) = w1 > b || (w1==b && n1 > a)  

formataLinha :: [String]-> String
formataLinha xs =  foldr (++) [] lista 
  where lista = map (\x-> x ++ "\n") xs

temNeg :: [Int] -> Bool 
temNeg xs = any aux xs 
  where aux x = x < 0 

difListas :: [Int] -> [Int] -> [Int]
difListas xs ys = filter (\x -> not (elem x ys)) xs 

somaNum :: [Int] -> [Int]
somaNum xs = zipWith (+) (filter odd xs) (filter even xs)

{-coletaTupla :: [Int] -> ([Int],[Int])
coletaTupla xs = (filter (\(a,b)-> b < pos indexa) indexa 
  where indexa = zip xs [1..]
        pos :: [(Int,Int)] -> (Int,Int)
        pos xs = head (filter (\(a,b)->a<0) xs)-}

coletaTupla1 :: [Int] -> ([Int],[Int])
coletaTupla1 xs = (positivos, negativos )
     where positivos = takeWhile (>0) xs
           negativos = dropWhile (>0) xs

total :: (Integer->Integer) -> Integer -> Integer
total f x = foldr (+) 0 (map f [0..x])

taNaLista :: Eq a => a -> [a] -> Bool
taNaLista x xs = or (map (==x) xs)

ulti :: Char -> String -> String
ulti x xs = filter (condicao) xs 
    where condicao a = a == x   
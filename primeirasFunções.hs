-- primeiras funções 
import Data.Char

dobro :: Int -> Int
dobro x = x*2

cond1 :: Bool -> Bool -> Bool
cond1 True False   =  False
cond1 _  _         =  True

tria:: Int->Int->Int->Bool
tria a b c = a+b >c && a+c>b && b+c>a 

exmp :: Float->Float->Float->Bool
exmp a b c 
 |a+b>c && b+c>a && c+a>b = True
 |otherwise = False

quadX :: Float -> Float 
quadX x = x*x

maxi :: Int->Int->Int
maxi m n 
 |m>=n = m
 |otherwise = n

maxiTres :: Int -> Int -> Int -> Int
maxiTres x y z
  | x >= y && x >= z = x
  | y >= x && y >= z = y
  | otherwise = z

maxiQuatro :: Int->Int->Int->Int->Int
maxiQuatro a b c d = maxi (maxi a b) (maxi c d)
    

media :: Float-> Float-> Float-> Float
media a b c = (a+b+c)/3

acima_media :: Float->Float->Float->Char
acima_media x y z
  |(x > media x y z) && (y> media x y z) && (z> media x y z)  = '3'
  |(x > media x y z && y> media x y z) || (x> media x y z && z> media x y z) || (y> media x y z && z > media x y z) = '2'
  |x > media x y z || y> media x y z || z > media x y z = '1'
  |otherwise = '0' 


iguais :: Int -> Int -> Int
iguais a b 
 |a==b = 2
 |otherwise = 0

iguais4 :: Int -> Int -> Int-> Int-> Int
iguais4 a b c d
  | a == b && a==c && a==d = 4 
  |(a /= b && b==c && b==d)||(a/=b && a==c && a==d)||(a==b && b/=c && a==d)||(a==b && a==c && a/=d) = 3
  |a/=b && a/=c && a/=d && b/=c && b/=d && c/=d = 0
  |otherwise = 2 


igual4 :: Int -> Int -> Int-> Int-> Int
igual4 a b c d 
  | a == b && a==c && a==d = 4 
  |(a /= b && b==c && b==d)||(a/=b && a==c && a==d)||(a==b && b/=c && a==d)||(a==b && a==c && a/=d) = 3
  |(a==b && b/=c && b/=d && c/=d)||(a==c && a/=b && a/=d && b/=d)||(a==d && a/=c && a/=b && b/=c)||(b==c && b/=a && b/=d && a/=d) = 2
  |(b==d && b/=a && b/=c && a/=c)||(c==d && c/=a && c/=b && a/=b) = 2
  |otherwise = 0 



igual3 :: Int -> Int -> Int -> Char
igual3 a b c
  |a==b && a==c = '3'
  |a==b && a/=c = '2'
  |b==c && a/=b = '2'
  |otherwise = '0'

diferentes :: Int -> Int -> Int -> Char
diferentes a b c 
  |a/=b && a/=c && b/= c = '3'
  |a/=b && a==c || c==b = '1'
  |b/=c && a==b || a==c = '1'
  |otherwise = '0'

quantos_pares :: Int-> Int -> Int -> Int -> Char
quantos_pares a b c d 
  |mod a 2 ==0 && mod b 2 ==0 && mod c 2== 0 && mod d 2 ==0 = '4'
  |(mod a 2 ==0 && mod b 2 ==0 && mod c 2== 0) || (mod b 2 ==0 && mod c 2== 0 && mod d 2 ==0) || (mod a 2 ==0 && mod b 2 ==0 && mod d 2 ==0)||(mod a 2==0 && mod c 2==0 && mod d 2==0) = '3'
  |(mod a 2==0 && mod b 2==0) || (mod a 2==0 && mod c 2==0) || (mod a 2==0 && mod d 2==0)|| (mod b 2==0 && mod c 2==0)||(mod b 2 ==0 && mod d 2==0)|| (mod c 2==0 && mod d 2 == 0) = '2'
  |(mod a 2==0) || (mod b 2==0) || (mod c 2==0) || (mod d 2== 0) = '1'
  |otherwise = '0' 

differ :: Int->Int->Int->Bool
differ a b c = a/=c && a/=b && b/=c 

menor_numero :: Int->Int->Int 
menor_numero a b 
  |a>b = b
  |otherwise = a

equaçao2grau :: Float->Float->Float->Float 
equaçao2grau a b c 
  |delta>0 = ((-1)*b + delta)/2*a
  |delta ==0 = (-1)*b/2*a
  |otherwise = error "a função não tem valor real"
  where delta = b^2 - 4*a*c 

dobrodasoma :: Int-> Int -> Int
dobrodasoma a b = 2* (a+b)

sqrprod :: Int->Int->Int
sqrprod a b = (a*b)*(a*b)

ou_exclusivo :: Bool->Bool->Bool
ou_exclusivo p q
  |p && q = False 
  |p == False && q == False = False
  |otherwise = True

myExOr :: Bool->Bool->Bool
myExOr True x = not x 
myExOr False x = x

nAnd :: Bool->Bool->Bool
nAnd True True = False
nAnd _ _= True

lucky :: Int->String
lucky 7 = "Voce acertou, sortudo!"
lucky _ = "Ops, tente outra vez!"

multiplo :: Int->String
multiplo x 
  |mod x 3 == 0 && mod x 5 == 0 = "multiplo de 3 e de 5"
  |mod x 3 == 0 && mod x 5 /= 0 = "multiplo de 3"
  |mod x 3 /= 0 && mod x 5 == 0 = "multiplo de 5"
  |otherwise = "nao e multiplo de 3 nem de 5"

retouquad :: Int->Int->String
retouquad x y 
  |x==y = "quadrado"
  |otherwise = "retangulo"

arearet :: Float->Float->Float
arearet x y = x*y

areacub :: Float->Float->Float->Float
areacub a b c = (arearet a b)*2 + a*2*c + b*c*2

areacub2 :: Float->Float->Float->Float
areacub2 a b c = (areat)*2 + a*2*c + b*c*2
 where areat = a*b

areacub3 :: Float->Float->Float->Float
areacub3 a b c = let area = a*b
                 in (area)*2 + a*2*c + b*c*2

significado :: String->String
significado "branco" = "paz" 
significado "amarelo" = "alegria"
significado "verde" = "esperança"
significado "azul" = "tranquilidade"
significado "vermelho" = "paixao"
significado _ = "desculpa"                

menorMaior :: Int->Int->(Int,Int)
menorMaior x y 
  |x<=y = (x,y)
  |otherwise = (y,x)

somaPar :: (Int, Int)-> Int
somaPar (0,y) = y
somaPar (x,y) = x+y

imc :: Float->Float->String
imc peso altura 
  |razao<magro = "Abaixo do peso"
  |razao<normal = "Peso normal"
  |razao<gordo = "Sobrepeso"
  |otherwise = "Obesidade"
  where razao = peso/altura^2
        (magro,normal,gordo) = (18.5,25.0,30.0)  

somaPar2 :: (Int,Int)->Int
somaPar2 p = (fst p+ snd p)

type Nome = String
type Idade = Int
type Altura = Float
type Pessoa = (Nome,(Idade,Altura))

nome :: Pessoa->Nome
nome pes = fst pes 

idade :: Pessoa->Int
idade pes = fst (snd pes)

altura :: Pessoa->Float
altura pes = snd (snd pes)

maxOcorre :: Int->Int->(Int,Int)
maxOcorre x y
  |x==y = (x,2)
  |otherwise = ((max x y),1)
  
ordem :: Int->Int->Int->(Int,Int,Int)
ordem x y z 
  |x>=y && x>=z && y>=z = (z,y,x)
  |x>=y && x>=z && y<=z = (y,z,x)
  |x<=y && x<=z && y>=z = (x,z,y)
  |x<=y && x>=z && y>=z = (z,x,y)
  |x<=y && x<=z && y<=z = (x,y,z)
  |otherwise = (y,x,z)

multQuatro :: [Int] -> [Int]
multQuatro lista = [x | x<-lista, mod x 4 == 0,x>=0]

negativo :: [Int] -> Bool
negativo lista = length [x | x<- lista, x<0] > 0

negativo1 :: [Int] -> Bool
negativo1 lista = or [x<0 | x<- lista]
 
negativo3 :: [Int]->Bool
negativo3 lista 
  |ls == [] = False
  |otherwise = True
  where ls= [x | x<-lista,x<0]

-- Elabore uma função que dada uma lista de alunos com suas notas, selecione os nomes dos alunos que obtiveram nota 
--igual ou  superior a 7.0, ordenados por faixa de notas (10, entre 9 e 9.9, entre 8 e 8.9, entre 7 e 7.9)

type Aluno = String
type Nota = Float
type NotasDisc = [(Aluno,Nota)] 

digits :: String->String
digits st = [ch | ch<-st, isDigit ch]
              
 
ehPar :: Int->Bool
ehPar x = mod x 2 == 0

impares :: [Int]->Bool
impares lista = ([]==[x | x<-lista,ehPar x])

tripImp :: [Int]->[Int]
tripImp trip = [x*3 | x<-trip, mod x 2/=0]

ehMinusculo :: Char->Bool
ehMinusculo c = ('a' <= c) && ('z'>= c)

paraMaisculo :: Char->Char
paraMaisculo c
  |ehMinusculo c = chr (ord c - ord 'a' + ord 'A')
  |otherwise = c 

stgMaisculo :: String -> String
stgMaisculo st = [ paraMaisculo x | x<-st] 

returnReais :: [Float]->[Float]
returnReais lista = [x | x<-lista, x>=0 && x<=100]

semDigito :: String->String
semDigito st = [x | x<-st, x/='0' && x/='1'&& x/='2'&& x/='3'&& x/='4'&& x/='5' && x/='6' && x/='7' && x/='8' && x/='9']

func :: Int-> Int
func x = farofa (dois (x + 4))

farofa :: Int-> Int
farofa a = a+1

dois :: Int-> Int
dois b = b+1

combi :: [String] -> [String] -> [String]
combi substantivos adjetivos = [substantivos ++" " ++ adjetivos| substantivos <-substantivos, adjetivos<-adjetivos]

maxOcorreTres :: Int->Int->Int->(Int,Int)
maxOcorreTres x y z 
  |fst maxXeY == x && fst maxXeZ == x = (x,(snd maxXeY + snd maxXeZ) - 1)
  |fst maxXeY == x = (z,1)
  |fst maxXeZ == x = (y,1)
  |otherwise = maxYeZ
  where maxXeY = maxOcorre x y
        maxXeZ = maxOcorre x z
        maxYeZ = maxOcorre y z

  

compr :: [Int]->Int
compr []= 0
compr lista = length lista    

estados_capitais :: [( String, String)] -> ([String], [String])
estados_capitais [(a,b)] = ([a], [b])

addNewLine :: String -> String
addNewLine palavra = palavra ++ "\n"


addNewLineLista :: [String] -> String
addNewLineLista lista = concat [addNewLine palavra | palavra <- lista]


addNewLinePrint :: [String] -> IO()
addNewLinePrint lista = putStr (addNewLineLista lista)

funcaobarra :: [String] -> [String]
funcaobarra xs = [ x ++ "\n" | x <- xs]

maxiCinco :: Int->Int->Int->Int->Int->Int
maxiCinco a b c d e = maxiTres (maxi a b) (maxi c d) e 

atualizaValor2 :: Int->[Int]->[Int]
atualizaValor2 i [] = []
atualizaValor2 i xs
  |indiceOK = parteUm ++ [valorAtual] ++ parteDois
  |otherwise = error "valor de i invalido"
   where indiceOK = (i > 0) && (i <= length xs)
         parteUm = take (i-1) xs
         valorAtual = dobro (xs!!(i-1))
         parteDois = drop i xs


adiciona :: [(Int,String,Int)] -> (Int,String,Int) -> [(Int,String,Int)]
adiciona x (a,y,z) = (a,y,z) : x
 

type CodProd = Int
type NomeProd = String
type PrecoProd = Int -- centavos
type Produto = (CodProd,NomeProd,PrecoProd)
type Menu = [Produto]

--cliente 
type CodCliente = Int
type NomeCliente = String
type CategCliente = Char
type ConsumoAnual = Int -- centavos
type Cliente = (CodCliente,NomeCliente,CategCliente,ConsumoAnual) 
type Clientes = [Cliente] 
type Compra = Int

-- pedido 
type Quant = Int
type SolCliente = (CodProd,NomeProd,Quant)
type PedidoCliente = [SolCliente]
type Pedidos = [(CodCliente,PedidoCliente)] 




cancelaProduto :: PedidoCliente -> CodProd -> Quant -> PedidoCliente
cancelaProduto pedidoCliente codigo quantidade 
   | head [c | (a,b,c)<-pedidoCliente] <= quantidade = [(a,b,c) | (a,b,c) <-pedidoCliente, codigo /= a] 
   | otherwise = [(a,b,c- quantidade) | (a,b,c) <- pedidoCliente, codigo == a, quantidade < c] ++ [(a,b,c) | (a,b,c) <-pedidoCliente, codigo /= a]   


serie :: (Int,Int) -> Float
serie (x,y) = aux6 x y

aux6 :: Int -> Int -> Float
aux6 x 0 = 0
aux6 x y 
  |mod y 2 /= 0 = (b/a) + aux6 x (y-1)
  |otherwise = (a/b) + aux6 x (y-1)
    where a = fromIntegral x :: Float
          b = fromIntegral y :: Float 


nomeord :: String -> [Int]
nomeord xs = map ord xs 

{-Elabore a função somaLI que dado uma lista de listas de inteiros não vazias
devolve a soma dos elementos das listas de inteiros que iniciaram com os números
1 e 100. Por exemplo,
somaLI [[1,2], [30, 4], [100,40,7],[20]]devolverá 150 (que é
(1+2)+(100+40+7))-}

somaLI :: [[Int]] -> Int 
somaLI xs = sum (concat [x | x<-xs, (head x) == 1 || (head x) == 100])

{-Elabore uma função semOrdem tal que dada uma lista de inteiros contendo pelo
menos um elemento par e outro ímpar, devolve True se os elementos pares e ímpares
aparecem misturados na lista, ou seja, quando não for o caso de todos os pares
ocorrerem antes do primeiro ímpar e também de todos os ímpares ocorrerem antes do

primeiro par na lista. Caso contrário, devolve False. (Dica: pense em termos de
posições dos elementos da lista). Por exemplo,
semOrdem [1,100,30,9,150,7] devolverá True
semOrdem [1,7,2,4] devolverá False
semOrdem [2,4,9,17] devolverá False-}

parimpar :: [Int] -> Bool
parimpar xs = ([x | x<-xs, even x] /= []) && ([x | x<-xs, odd x] /= []) 
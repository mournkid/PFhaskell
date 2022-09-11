{-class Eq a where
(==), (/=) :: a -> a -> Bool
x /= y = not (x==y)

instance Eq Bool where
(==) :: a -> a -> Bool
True == True = True
False == False = True
_ == _ = False-}

import Data.Char

todosIguais :: Eq a => a-> a -> a -> Bool
todosIguais m n p = (m==n) && (n==p)

{-class Eq a => Ord a where
(<), (<=), (>), (>=) :: a -> a -> Bool
max, min :: a -> a -> a
compare :: a -> a -> Ordering-}

insOrd :: Ord a => a -> [a] -> [a]
insOrd y [] = [y]
insOrd y (z:zs)
  | y <= z = y : z : zs
  | otherwise = z: insOrd y zs

ordInsercao :: Ord a => [a] -> [a]
ordInsercao [] = []
ordInsercao (x:xs) = insOrd x (ordInsercao xs)

maxi :: Ord a => a -> a -> a 
maxi x y 
 | x > y = x 
 | otherwise = y 

maxTres :: Ord a => a -> a -> a -> a 
maxTres x y z = max x (max y z)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = foldr aux [] (x:xs)
  where aux :: Ord a => a -> [a] -> [a]
        aux x xs = filter menor xs ++ [x] ++ filter maior xs 
          where menor y = y <= x 
                maior y = y > x 

ordSelecao :: Ord a => [a] -> [a]
ordSelecao [] = []
ordSelecao zs = m : ordSelecao (removeLista zs m)
  where m = menor1 zs

removeLista :: Ord a => [a] -> a -> [a]
removeLista [] _ = []
removeLista (x:xs) n 
  | x == n = xs 
  |otherwise = x : removeLista xs n 

menor1 :: Ord a => [a] -> a
menor1 [x] = x
menor1 (x:xs) = min x (menor1 xs) 

taNaLista :: Eq a => a -> [a] -> Bool
taNaLista _ [] = False
taNaLista x (y:ys) = x == y || taNaLista x ys 

-- exercicio recomendado do slide 17 
-- checagem de funções slide 22 
-- maxBound e minBound 

data Ponto2D = Ponto Int Int deriving (Show)

deslocaX :: Ponto2D -> Int -> Ponto2D
deslocaX (Ponto x y) delta = Ponto (x + delta) y

type Nome = String
type Idade = Int
{-data Individuo = Pessoa Nome Idade 

nome :: Individuo -> Nome
nome (Pessoa n _) = n 

idade :: Individuo -> Idade
idade (Pessoa _ y) = y-}

data Formato = Circulo Float
              |Retangulo Float Float
              deriving (Eq, Show, Read, Ord)

redondo :: Formato -> Bool
redondo (Circulo _ ) = True
redondo _ = False

area :: Formato -> Float
area (Circulo r) = pi * r ^ 2
area (Retangulo a b) = a * b

data Estacao = Primavera | Verao | Outono | Inverno deriving (Eq, Ord, Enum, Show, Read)
type Pessoa1 = String
type Livro = String
type Emprestimos = [Emprestimo]

data Emprestimo = LivroPessoa (String,String) deriving (Show,Eq)

rmvLista :: Emprestimo -> Emprestimos -> Emprestimos
rmvLista _ [] = []
rmvLista x (b:bs) 
  | x == b = rmvLista x bs
  |otherwise = b : rmvLista x bs


data Expr = Lit Integer
  |Add Expr Expr
  |Sub Expr Expr

avalia:: Expr1 -> Integer
avalia (Lit1 n) = n
avalia (e1 :+: e2) = (avalia e1) + (avalia e2)
avalia (e1 :-: e2) = (avalia e1) - (avalia e2)
avalia (e1 :*: e2) = (avalia e1)*(avalia e2)

show1 :: Expr1 -> String
show1 (Lit1 n) = show n
show1 (e1 :+: e2) = "(" ++ show1 e1 ++ "+" ++ show1 e2 ++ ")"
show1 (e1 :-: e2) = "(" ++ show1 e1 ++ "-" ++ show1 e2 ++ ")"
show1 (e1 :*: e2) = "(" ++ show1 e1 ++ "*" ++ show1 e2 ++ ")"

instance Show Expr where
  show (Lit n) = show n
  show (Add e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Sub e1 e2) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"

data Expr1 = Lit1 Integer 
  |Expr1 :+: Expr1
  |Expr1 :-: Expr1 
  |Expr1 :*: Expr1


somaArv :: Arv -> Integer
somaArv NoNulo = 0 
somaArv (No x t1 t2) = x + somaArv t1 + somaArv t2

ocorreArv :: Arv -> Integer -> Integer
ocorreArv NoNulo _ = 0 
ocorreArv (No x t1 t2) y 
  | x == y = 1 + ocorreArv t1 y + ocorreArv t2 y
  | otherwise = ocorreArv t1 y + ocorreArv t2 y

type Endereco = String

data Pessoa = Adulto Nome Endereco Biog
  | Crianca Nome


data Biog = Pai String [Pessoa]
  | NaoPai String

-- duvida contOp pag 24 aula 18 

data Ops = Adds | Subs | Mult
data Expr2 = Lit2 Integer | Op Ops Expr2 Expr2

avalia1 :: Expr2 -> Integer
avalia1 (Lit2 n) = n
avalia1 (Op Adds e1 e2) = (avalia1 e1) + (avalia1 e2)
avalia1 (Op Subs e1 e2) = (avalia1 e1) - (avalia1 e2)
avalia1 (Op Mult e1 e2) = (avalia1 e1)*(avalia1 e2)

show2 :: Expr2 -> String
show2 (Lit2 n) = show n
show2 (Op Adds e1 e2) = "(" ++ show2 e1 ++ "+" ++ show2 e2 ++ ")"
show2 (Op Subs e1 e2) = "(" ++ show2 e1 ++ "-" ++ show2 e2 ++ ")"
show2 (Op Mult e1 e2) = "(" ++ show2 e1 ++ "*" ++ show2 e2 ++ ")"


data Arv1 = NoNulo1
  |No1 Integer Arv1 

retrnLft :: Arv1 -> Integer
retrnLft NoNulo1 = 0 
retrnLft (No1 x y) = x 

aparece :: Arv -> Integer -> Bool 
aparece NoNulo 0 = True
aparece NoNulo _ = False
aparece (No x e1 e2) n = n == x || aparece e1 n || aparece e2 n  

maior :: Arv -> Integer
maior NoNulo = 0
maior (No x e1 e2) = max x (max (maior e1) (maior e2))

menor :: Arv -> Integer 
menor NoNulo = 0 
menor (No x e1 e2) = min x (min (menor e1) (menor e2))
 

maiorMenor :: Arv -> (Integer,Integer)
maiorMenor x = (maior x, menor x)

troca :: Arv -> Arv
troca NoNulo = NoNulo 
troca (No x e1 e2) = No x e2 e1 

data ListInt = Vazia
  | Cons Int ListInt deriving (Show)

tamLista :: ListInt -> Int
tamLista Vazia = 0
tamLista (Cons x xs) = 1 + tamLista xs


concatenaLista :: ListInt -> ListInt -> ListInt
concatenaLista Vazia xs = xs
concatenaLista (Cons y ys) xs = Cons y (concatenaLista ys xs)

data List a = Vazia1
  | Cons1 a (List a)
  deriving (Eq, Ord, Show, Read)

data Arv2 a = NoNulo2
  |No2 a (Arv2 a) (Arv2 a)
  deriving (Eq, Ord, Show, Read)

data Meses = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro deriving (Eq,Show,Ord)

data Dia = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado deriving (Eq) 

data Cliente = Pessoa Nome Meses deriving (Show,Eq)

cadastro :: [Cliente]
cadastro = [Pessoa "Leila" Novembro, Pessoa "Maria" Maio, Pessoa "Joana" Maio]

type Preco = Float 
type MesCorrente = Meses 

desconto :: Preco -> Nome -> Dia -> MesCorrente -> [Cliente] -> Preco
desconto preco nome dia mes clientes = (preco - (preco*(desconto dia mes clientes))) 
  where desconto :: Dia -> MesCorrente -> [Cliente] -> Preco 
        desconto x y xs 
          | aniver xs y /= [] = 0.5 
          | x == Segunda || x == Terca = 0.3 
          | x == Quarta || x == Quinta = 0.15 
          | x == Sexta || x == Sabado = 0 
          | otherwise = error "fechado"
        aniver :: [Cliente] -> MesCorrente -> [Cliente]
        aniver xs x = filter (\(Pessoa n m)-> m==x) xs 

aniversariantes :: Meses -> [Cliente] -> [Nome]
aniversariantes mes clientes = map (\(Pessoa x y)-> x) (filter (\(Pessoa x y)-> y==mes) clientes)

removeCl :: Nome -> [Cliente] -> [Cliente]
removeCl nome xs = filter (\(Pessoa x y)-> x /= nome) xs 

ordeneCl :: [Cliente] -> [Cliente]
ordeneCl xs = foldr insOrd [] xs
  where insOrd :: Cliente -> [Cliente] -> [Cliente]
        insOrd (Pessoa a b) xs = takeWhile menorz xs ++ [(Pessoa a b)] ++ dropWhile menorz xs
          where menorz (Pessoa x y) = y < b

data Arv  = NoNulo 
  |No Integer Arv  Arv  deriving (Show,Eq,Read,Ord)

colapsaArv :: Arv  -> [Integer]
colapsaArv NoNulo = []
colapsaArv (No x e1 e2) = colapsaArv e1 ++ [x] ++ colapsaArv e2 

data Eithr a b = Lft a | Rght b
 deriving (Eq, Ord, Show, Read)

isLeft :: Eithr a b -> Bool
isLeft (Lft _) = True
isLeft _ = False

duasFunc :: (a->c) -> (b->c) -> Eithr a b -> c
duasFunc f g (Lft x) = f x
duasFunc f g (Rght y) = g y

data Tree a = Nulo 
  | Na a (Tree a) (Tree a) deriving (Show,Eq,Read,Ord)  

{-mapTree :: (a->b) -> Arv a -> Arv b
mapTree f NoNulo = NoNulo
mapTree f (No x e1 e2) = No (f x) (mapTree f e1) (mapTree f e2)-}

troca1 :: Eithr a b -> Eithr b a
troca1 (Lft a ) = Rght a 
troca1 (Rght a) = Lft a 

{-maior1 :: Arv -> Int 
maior1 NoNulo = 0
maior1 (No x e1 e2) = max x (maior1 e1)

maior2 :: Arv -> Int 
maior2 NoNulo = error "arvore vazia"
maior2 (No x e1 e2) = max x (maior2 e2)

maiorx :: Arv -> Int 
maiorx (No x e1 e2) = max (maior1 e1) (maior2 e2)
-}

checaMaxMinNo :: Arv -> (Integer,Integer)
checaMaxMinNo NoNulo = error "inexistente"
checaMaxMinNo (No x NoNulo NoNulo) = (x,x)
checaMaxMinNo (No x tesq tdir) = (min x (fst (checaMaxMinNo tesq)), max x (snd (checaMaxMinNo tdir))) 
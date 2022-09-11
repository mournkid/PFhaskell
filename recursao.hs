pot :: Float -> Int -> Float
pot _ 0 = 1  -- caso base 
pot a k = a * pot a (k-1) -- caso geral

pott :: Float -> Int -> Float
pott a k 
  |k < 0 = error "expoente negativo"
  |otherwise = pot a k 


meuLength :: [Int] -> Int
meuLength [] = 0
meuLength (x:xs) = 1 + meuLength xs 

adjetivos :: [String] -> [String] -> [String]
adjetivos [] [] = []
adjetivos (x:xs) (y:ys) = (x ++ " " ++ y) : adjetivos xs ys 

listaSum :: [Int] -> Int
listaSum [] = 0
listaSum (x:xs) = x + listaSum xs

fatorial :: Int -> Int 
fatorial 0 = 1
fatorial x = fatorial (x-1) * x 

 
 


miguelito :: Int -> Int
miguelito  = 3
miguelito x
  | mod x 2 == 0 = 4 + miguelito (x-1)
  | otherwise = 1 + miguelito(x-1)

fatorial1 :: Int -> Int
fatorial1 x
  | x < 0 = error "numero negativo"
  | otherwise = fatorial x 

listaProd :: [Int] -> Int
listaProd [] = 1
listaProd (x:xs) = x * listaProd xs 

meuConcat :: [[Int]] -> [Int]
meuConcat [] = [] 
meuConcat (xs:xss) = xs ++ meuConcat xss

meuReverse :: [Int] -> [Int]
meuReverse [] = []
meuReverse (x:xs) = meuReverse xs ++ [x]

meuZip :: [Int] -> [Int] -> [(Int,Int)]
meuZip _ [] = []
meuZip [] _ = []
meuZip (x:xs) (y:ys) = (x,y) : meuZip xs ys 

meuTake :: Int -> [Int] -> [Int]
meuTake _ [] = []
meuTake 0 _ = []
meuTake n (x:xs) = x : meuTake (n-1) xs 

meuTakeGeral :: Int -> [Int] -> [Int]
meuTakeGeral n ls
  | n < 0 = []
  | otherwise = meuTake n ls

coletaPares :: [Int] -> [Int]
coletaPares [] = []
coletaPares (x:xs) 
  | ehPar x = x: coletaPares xs
  |otherwise = coletaPares xs 
    where ehPar x = mod x 2 == 0 

meuReplicate :: Int -> [Int] -> [[Int]]
meuReplicate 0 _ = []
meuReplicate n xs = [xs] ++  (meuReplicate (n-1) xs)

meuReplicate1 :: Int -> [Int] -> [[Int]]
meuReplicate1 n xs 
  | n < 0 = []
  |otherwise = meuReplicate n xs 

meuReplic :: Int -> Int -> [Int]
meuReplic 0 _ = []
meuReplic n x = x : meuReplic (n-1) x 

replica :: Int -> Int -> [Int]
replica n x 
  | n < 0 = []
  |otherwise = meuReplic n x 

meuAnd :: [Bool] -> Bool
meuAnd [] = True 
meuAnd (x:xs) = x && meuAnd xs

somaPares :: [Int] -> Int
somaPares [] = 0
somaPares (x:xs)
  |ehPar x = x + somaPares xs 
  |otherwise = somaPares xs 
    where ehPar x = mod x 2 == 0 


meuOr :: [Bool] -> Bool
meuOr [] = False
meuOr (x:xs) = x || meuOr xs 

meuDrop :: Int -> [Int] -> [Int]
meuDrop _ [] = []
meuDrop n (x:xs) 
  | n <= 0 = (x:xs)
  | otherwise = meuDrop (n-1) xs   

{-meuUnZip :: [(Int,Int)] -> ([Int],[Int])
meuUnZip [] = ([],[])
meuUnZip ((a, b):xs) = (a : fst (meuUnZip xs), b : snd (meuUnZip xs))
 -}
  

soma1 :: [Int] -> [Int]
soma1 [] = []
soma1 (x:xs) = (x+1) : soma1 xs 


digitosString :: String -> String
digitosString "" = ""
digitosString (x:xs) 
  |ehDigito x = x :digitosString xs
  |otherwise  = digitosString xs
      where ehDigito :: Char -> Bool
            ehDigito x
             |x == '0' = True
             |x == '1' = True
             |x == '2' = True
             |x == '3' = True
             |x == '4' = True
             |x == '5' = True
             |x == '6' = True
             |x == '7' = True
             |x == '8' = True
             |x == '9' = True
             |otherwise = False

taNaLista :: Int -> [Int] -> Bool
taNaLista _ [] = False
taNaLista n (x:xs) =  n == x || taNaLista n xs

elemIguais :: [Int] -> Bool
elemIguais [] = True
elemIguais [x] = True
elemIguais (x:xs) = x == head xs && elemIguais xs 

prodImpares :: [Int] -> Int
prodImpares [] = 1
prodImpares (x:xs)
  |mod x 2 /= 0 =  x * prodImpares xs 
  |otherwise = prodImpares xs 

{-Elabore uma função recursiva elemNum para dados uma lista de inteiros
e um número inteiro, devolver a quantidade de vezes que este número
ocorre na lista.-}
removerElem :: Int -> [Int] -> [Int]
removerElem _ [] = []
removerElem n (x:xs) 
  | x==n = removerElem n xs 
  |otherwise = x : removerElem n xs 

elemNum :: [Int] -> Int -> Int
elemNum [] _ = 0
elemNum (x:xs) n 
  |n == x = 1 + elemNum xs n 
  |otherwise = elemNum xs n  

{-unicos :: [Int] -> [Int]
unicos [] = []
unicos (l:ls) 
  |elemNum (l:ls) l > 1 = unicos (remove ls l)  
  |otherwise = l : unicos ls
    where remove :: [Int] -> Int -> [Int]
          remove [] _ = []
          remove (x:xs) n 
            |x == n  = remove xs n
            |otherwise = x : remove xs n

unicos :: [Int] -> [Int]
unicos [] = []
unicos (l:ls) 
  |elemNum (l:ls) l > 1 = unicos ls   
  |otherwise = l : unicos ls-}
  

type Pessoa = String
type Livro = String
type Emprestimos = [(Pessoa, Livro)]

livros :: Pessoa -> Emprestimos -> [Livro]
livros _ [] = []
livros nome ((a,b):xs) 
  |a == nome = b : livros nome xs 
  |otherwise = livros nome xs 

donx :: Livro -> Emprestimos -> [Pessoa] 
donx _ [] = []
donx livro ((a,b):xs) 
  |b == livro = a : donx livro xs 
  |otherwise = donx livro xs 

{-Dado um livro, desejamos saber se o mesmo se encontra emprestado ou não;-}

emprestado :: Livro -> Emprestimos -> Bool
emprestado _ [] = False
emprestado livro ((a,b):xs) = livro == b || emprestado livro xs  

{-Dada uma pessoa, desejamos saber a quantidade de livros que ela tomou emprestado;-}

quantidade :: Pessoa -> Emprestimos -> Int
quantidade _ [] = 0
quantidade pessoa ((a,b):xs) 
  | pessoa == a = 1 + quantidade pessoa xs
  | otherwise = quantidade pessoa xs 
       
{-Dado um par (Pessoa, Livro), queremos removê-lo da lista de emprestados, sinalizando a
sua devolução.-}

rmvLista :: (Pessoa,Livro) -> Emprestimos -> Emprestimos
rmvLista _ [] = []
rmvLista n (x:xs) 
  |n == x = rmvLista n xs
  |otherwise = x : rmvLista n xs 

maximo :: [Int] -> Int
maximo [x] = x
maximo [] = error "lista invalida"
maximo (x:xs) = max x (maximo xs) 

posChar :: Char -> [Char] -> Int
posChar y xs = ocorre y (indexaLista xs)
  where indexaLista xs = zip [1..] xs
        ocorre :: Char -> [(Int,Char)] -> Int
        ocorre z [] = -1
        ocorre z (y:ys) 
          |z == snd y = fst y
          |otherwise = ocorre z ys 

-- multiplicar os elementos de uma lista pelos de outra 
--[2,4] [10,20,30] = [20,40,60,40,80,120]

multLista :: [Int] -> [Int] -> [Int]
multLista [] _ = []                            
multLista _ [] = []
multLista (x:xs) ys = auxiliar x ys ++ multLista xs ys
       where auxiliar :: Int -> [Int] -> [Int]
             auxiliar _ [] = []
             auxiliar x (y:ys) = x * y : auxiliar x ys


-- INDUÇÃO FORTE

fib :: Int -> Int
fib k
 |k < 0 = error "indefinido para numero negativo"
 |k == 0 = 0
 |k == 1 = 1
 |otherwise = fib (k-1) + fib(k-2)

potencia :: Float -> Int -> Float
potencia a k
  | k < 0 = error "exponente negativo"
  | k == 0 = 1
  | k == 1 = a
  | mod k 2 == 0 = (potencia a (div k 2))^2
  | otherwise = a* (potencia a (div k 2))^2

palindromo :: String -> Bool
palindromo [] = True
palindromo [x] = True
palindromo (x:xs) = x == last xs && palindromo (init xs)

{-par1 :: Int -> Bool
par n
 |n == 0 = True
 |n == 1 = False
 |otherwise = par (n-2)-}

meuDiv :: Int -> Int -> Int
meuDiv x y 
  |y == 0 = error "nao existe"
  |x < y = 0
  |otherwise = 1 + meuDiv (x-y) y

meuMod :: Int -> Int -> Int
meuMod x y 
  |y == 0 = error "nao existe"
  |x < y = x 
  |otherwise = meuMod (x-y) y 

divMod1 :: Int -> Int -> (Int,Int)
divMod1 x y = (quoc x y, resto x y)
   where quoc :: Int->Int->Int
         quoc x y 
           |y == 0 = error "nao existe"
           |x < y = 0 
           |otherwise = 1 + quoc (x-y) y 
         resto :: Int->Int->Int
         resto x y
           |y == 0 = error "nao existe"
           |x < y = x 
           |otherwise = resto (x-y) y 

par :: Int -> Bool
par 0 = True
par n = impar (n-1)

impar :: Int -> Bool
impar 0 = False
impar n = par (n-1)

fibTupla :: Int -> (Int, Int)
fibTupla 0 = (0,1)
fibTupla n = (b, a+b)
  where (a, b) = fibTupla (n-1) -- DUVIDA

prodMult :: [Int] -> Int -> Int
prodMult (x:xs) z 
  | z < 0 = error "posição invalida"
  | z == 0 = 1
  | pos (indexa (x:xs)) z /= [] = product  (pos (indexa (x:xs)) z)
  | otherwise = prodMult xs z 
    where indexa xs = zip xs [1..]
          pos :: [(Int,Int)] -> Int -> [Int]
          pos [] _ = []
          pos ((a,b):xs) z 
            |mod b z == 0 = a : pos xs z 
            |otherwise = pos xs z 

palavra :: String -> String
palavra [] = []
palavra (x:xs)
  |x == ' ' = palavra xs 
  |otherwise = (x:xs)

palavraAux :: String -> String 
palavraAux [] = []
palavraAux (x:xs)
  |x == ' ' = []
  |otherwise = x : palavraAux xs

palavra2 :: String -> [String]
palavra2 [] = []
palavra2 [x] = [[x]]
palavra2 (x:xs)
  |x == ' ' = palavra2 xs  
  |otherwise = palavraAux (x:xs) : palavra2 (drop comp xs)
     where comp = length (palavraAux (x:xs))

ocorreNoTexto :: String -> String -> Bool
ocorreNoTexto _ [] = False
ocorreNoTexto xs (y:ys) = xs == head (palavra2 (y:ys)) || ocorreNoTexto xs ys

qntsXocorre :: String -> [String] -> Int
qntsXocorre _ [] = 0
qntsXocorre xs (y:ys)
  | xs == y = 1 + qntsXocorre xs ys 
  | otherwise = qntsXocorre xs ys

ocorre :: String -> String -> Int
ocorre xs ys = qntsXocorre xs (palavra2 ys) 

{-ocorre1 :: String -> String -> Int 
ocorre1 [] _ = 0
ocorre1 _ [] = 0
ocorre1 (x:xs) ys 
  | ocorreNoTexto ys (x:xs) = 1 + ocorre1 
  | otherwise = ocorre1  ys
-}
break2 :: String -> Char -> String 
break2 (x:xs) char = drop (pos indexa char) (x:xs)
          where indexa = zip (x:xs) [1..]
                pos :: [(Char,Int)] -> Char -> Int 
                pos [] _ = 0 
                pos ((a,b):xs) char 
                  |a == char = b 
                  |otherwise = pos xs char  


break1 :: String -> Char -> String 
break1 (x:xs) char = take ((pos indexa char)-1) (x:xs)
          where indexa = zip (x:xs) [1..]
                pos :: [(Char,Int)] -> Char -> Int 
                pos [] _ = 0 
                pos ((a,b):xs) char 
                  |a == char = b 
                  |otherwise = pos xs char  

trocar :: String -> String -> String -> [String]
trocar a b texto = inicio ++ [termo] ++ final
         where indexa = zip (palavra2 texto) [1..]
               inicio = take ((posicao indexa a)-1) (palavra2 texto) 
               termo = atualiza (palavra2 texto !! ((posicao indexa a) - 1)) b 
               final = drop (posicao indexa a) (palavra2 texto)
               posicao :: [(String,Int)] -> String -> Int
               posicao [] _ = 0 
               posicao _ [] = 0 
               posicao ((a,b):xs) palavra
                 |a == palavra = b 
                 |otherwise = posicao xs palavra 
               atualiza palavra termo = termo 

acrescentaEspaço :: [String] -> String
acrescentaEspaço [] = []
acrescentaEspaço (x:xs) = x ++ " " ++ acrescentaEspaço xs 

-- ORDENAÇÃO

mini :: Int -> Int -> Int
mini a b = if a<=b then a else b 

menor :: [Int] -> Int
menor [x] = x
menor (x:xs) = mini x (menor xs)

removeLista :: [Int] -> Int -> [Int]
removeLista [] _ = []
removeLista (x:xs) n 
  | x == n = xs 
  |otherwise = x : removeLista xs n 

ordSelecao :: [Int] -> [Int]
ordSelecao [] = []
ordSelecao zs = m:ordSelecao (removeLista zs m)
  where m = menor zs 

palavraigual :: String -> String -> Bool
palavraigual [] [] = True
palavraigual _ [] = False
palavraigual [] _ = False
palavraigual (x:xs) (y:ys) = x == y && palavraigual xs ys 

subNConsec :: String -> String -> Bool
subNConsec [] _ = True
subNConsec _ [] = False
subNConsec (x:xs) (y:ys)
  | x == y = subNConsec xs ys 
  |otherwise = subNConsec (x:xs) ys 
   
multiplos :: [Int] -> ([Int],[Int],[Int])
multiplos xs = multAux (reverse xs) ([],[],[])
  where multiplo n y = y >= 0 && mod y n == 0
        multAux :: [Int] -> ([Int],[Int],[Int]) -> ([Int],[Int],[Int])
        multAux [] t = t
        multAux (y:ys) (m3s,m5s,m35s)
          |multiplo 3 y && multiplo 5 y = multAux ys (y:m3s,y:m5s,y:m35s)
          |multiplo 3 y = multAux ys (y:m3s,m5s,m35s)
          |multiplo 5 y = multAux ys (m3s,y:m5s,m35s)
          |otherwise = multAux ys (m3s,m5s,m35s)
  

-- INSERÇÃO DIRETA

insOrd :: Int -> [Int] -> [Int]
insOrd y [] = [y] 
insOrd y (z:zs) 
  | y <= z = y : z : zs
  | otherwise = z: insOrd y zs


ordInsercao :: [Int] -> [Int]
ordInsercao [] = [] 
ordInsercao (x:xs) = insOrd x (ordInsercao xs) 

-- MERGE SORT 

intercala :: [Int] -> [Int] -> [Int]
intercala xs [] = xs 
intercala [] ys = ys
intercala (x:xs) (y:ys) 
  | x <= y = x: intercala xs (y:ys)
  | otherwise = y: intercala (x:xs) ys

mergeSort :: [Int] -> [Int]
mergeSort [] = [] 
mergeSort [x] = [x]
mergeSort xs = intercala (mergeSort us) (mergeSort vs) 
     where meio = (length xs) `div` 2
           us = take meio xs
           vs = drop meio xs

-- QUICK SORT 

quickSort :: [Int] -> [Int]
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
             |otherwise = vs ys x 


type LinhaFig = [Char]
type Figura = [LinhaFig]

inverteCh :: Char -> Char
inverteCh x = if x == '1' then '.' else '1'

inverteCorLin :: LinhaFig -> LinhaFig
inverteCorLin [] = []
inverteCorLin (x:xs) = inverteCh x : inverteCorLin xs 

inverteCorFig :: Figura -> Figura
inverteCorFig [] = []
inverteCorFig (xs:xss) = inverteCorLin xs:inverteCorFig xss

refleteH :: Figura -> Figura
refleteH [] = []
refleteH xss = reverse xss

ladoAlado :: Figura -> Figura
ladoAlado [] = []
ladoAlado (xs:xss) = (xs++xs): ladoAlado xss

escalaFig :: Int -> Figura -> Figura
escalaFig _ [] = []
escalaFig k (xs:xss)
  | k <= 0 = []
  |otherwise = replicaLin k concatenar ++ escalaFig k xss
     where linEsc = replicate k xs 
           concatenar = concat linEsc  
           replicaLin :: Int -> LinhaFig -> Figura
           replicaLin 0 us = []
           replicaLin i us = us: replicaLin (i-1) us

disjuntas :: [Int] -> [Int] -> Bool
disjuntas [] [] = True
disjuntas _ [] = True
disjuntas [] _ = True
disjuntas (x:xs) (y:ys) = auxiliar x (y:ys) && disjuntas xs (y:ys) 
      where auxiliar :: Int -> [Int] -> Bool
            auxiliar _ [] = True
            auxiliar n (y:ys)
              |n /= y && auxiliar n ys = True
              |otherwise = False


elementosIguais :: [Int] -> [Int] -> [Int]
elementosIguais [] _ = []
elementosIguais _ [] = []
elementosIguais (x:xs) (y:ys) 
  |pertence x (y:ys) = x : elementosIguais xs (y:ys) 
  |otherwise = elementosIguais xs (y:ys) 
    where pertence :: Int -> [Int] -> Bool
          pertence _ [] = False
          pertence n (y:ys) = n == y || pertence n ys 

elementosIguais1 :: [Int] -> [Int] -> [Int]
elementosIguais1 [] _ = []
elementosIguais1 _ [] = []
elementosIguais1 xs ys = filtra (quickSort (xs ++ ys))
           where filtra [] = []
                 filtra [x] = []
                 filtra (z:t:zs)
                   |z == t = z: filtra zs
                   |otherwise = filtra (t:zs)


somaDigitos :: Int -> Int
somaDigitos x = auxiliar (show x)
     where auxiliar :: String -> Int
           auxiliar [] = 0
           auxiliar (x:xs) = read [x]  + auxiliar xs


ehDigito1 :: String -> String
ehDigito1 "" = ""
ehDigito1 (x:xs)
  | (x >= '0') && (x <= '9') = [x] ++ ehDigito1 xs
  | otherwise = ehDigito1 xs

-- arroz xs = drop ((length xs) - 2) xs 
-- arroz "850" = "50"

{-quickSort :: [(Int,String,Int)] -> [(Int,String,Int)]
quickSort [] = [] 
quickSort ((codigo,nome,preco):xs) = (quickSort (us xs codigo))  ++ [(codigo,nome,preco)] ++ (quickSort (vs xs codigo))
     where us :: [(Int,String,Int)] -> Int -> [(Int,String,Int)]
           us [] _ = []
           us ((codigo,nome,preco):ys) x 
             |codigo <= x = (codigo,nome,preco) : us ys x 
             |otherwise = us ys x 
           vs :: [(Int,String,Int)] -> Int -> [(Int,String,Int)]
           vs [] _ = []
           vs ((codigo,nome,preco):ys) x 
             |codigo > x = (codigo,nome,preco) : vs ys x 
             |otherwise = vs ys x-}

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


codProdutos :: Produto -> CodProd
codProdutos (x, _, _) = x

{-adicionaProdMenu :: Menu -> Produto -> Menu
adicionaProdMenu menu produto
        |jaExiste = 
        |otherwise = menu ++ [produto]
        where jaExiste = elem (codProdutos produto) (codLista menu) 
              codLista ::  Menu -> [CodProd]
              codLista [] = []
              codLista ((a,b,c):xs) = a : codLista xs -}


multiplo_tres :: Int
multiplo_tres = sum (filter (\x-> (mod x 2)/=0) (lista 500)) 
  where lista :: Int -> [Int]
        lista x = [3,6..x] 


meuUnZip :: [(Int,Int)] -> ([Int],[Int])
meuUnZip [] = ([],[])
meuUnZip xs = (auxiliarx xs, auxiliary xs)
  where auxiliarx :: [(Int,Int)] -> [Int]
        auxiliarx [] = []
        auxiliarx ((x,y):xys) = x: auxiliarx xys
        auxiliary :: [(Int,Int)] -> [Int]
        auxiliary [] = []
        auxiliary ((x,y):xys) = y: auxiliary xys

mdc::Int->Int->Int
mdc a b 
 | a < b = mdc b a
 | b == 0 = a
 | otherwise = mdc b (mod a b)


mmc::Int->Int->Int
mmc x y = (x * y) `div` (mdc x y)
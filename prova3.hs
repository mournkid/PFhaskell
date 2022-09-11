-- Camille Sousa Meneses de Santana 
-- 202000024285

import Data.List 
import Data.Char 

type NomeProd = String
type Preco = Float
type Fornecedor = String
type Fornecedores = [Fornecedor]
type FornPrec = (Fornecedor, Preco)
type Produto = (NomeProd, [FornPrec])
type Produtos = [Produto]
 

listaFornecedores :: Produto -> Fornecedores
listaFornecedores produto = map fst fornprec
      where fornprec = snd produto 

removeFornecedores :: Produto -> Fornecedores -> Produto
removeFornecedores produto fornecedores = (fst produto, filter (\(a,b)-> (notElem a fornecedores)) fornprec)
  where fornprec = snd produto 




maiorMenorPreco :: Produto -> (NomeProd, FornPrec, FornPrec)
maiorMenorPreco produto 
  |snd produto == [] = error "fornecedor inexistente" 
  |otherwise = (fst produto,(fornecedor maior fornprec),(fornecedor menor fornprec))
      where maior = foldr1 max (map (\(a,b)-> b) fornprec)
            menor = foldr1 min (map (\(a,b)-> b) fornprec)
            fornprec = snd produto
            fornecedor :: Preco -> [FornPrec] -> FornPrec
            fornecedor preco lista = aux (filter (\(a,b)-> b==preco) lista) 
            aux (x:xs) = x 
{-getInt :: IO Int 
getInt = do line <- getLine 
            return (read line :: Int)

copyN :: Int -> [String] -> IO [String]
copyN n str = if n<=0
               then return str 
               else do line <- getLine
	                   copyN (n-1) (words line ++ str)

frases :: IO()
frases = do putStrLn ("Digite o numero de frases:")
            n <- getInt
            putStrLn ("Digite as frases:")
            palavras <- copyN n []
            putStrLn ("Palavra mais longa:" ++ palavra palavras)

palavra :: [String] -> String 
palavra xs = maximum xs-} 

data Arv a = NoNulo | No a (Arv a) (Arv a) deriving (Ord,Eq)

altura :: Arv a -> Int
altura NoNulo = -1 
altura (No x tesq tdir) = 1 + altura tdir

type Disciplina = String
type Nota = Float 
type Nome = String
type Aluno = (Nome, Nota)
type Alunos = [Aluno]
type Turma = (Disciplina, Alunos)
type Oferta = [Turma]

listaAlunosTurmas :: Oferta -> [(Disciplina,[Nome])]
listaAlunosTurmas oferta = map aux oferta  
  where aux :: Turma -> (Disciplina,[Nome])
        aux turma = (fst turma,(map (\(a,b)->a) aux1))
         where aux1 = snd turma

positivos :: Arv Int -> [Int]
positivos NoNulo = []
positivos (No x tesq tdir) = filter (\x-> x>=0) ((positivos tesq) ++ [x] ++ (positivos tdir))


parImpar :: Int -> String 
parImpar x 
  |mod x 2 == 0 = "par"
  |otherwise = "impar"

getInt :: IO Int 
getInt = do line <- getLine 
            return (read line :: Int)

paridade :: IO() 
paridade = do putStrLn ("Digite um numero:") 
              line <- getInt
              putStrLn ("Paridade: " ++ parImpar line)
              paridade  
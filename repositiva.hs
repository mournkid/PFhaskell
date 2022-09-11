-- Camille Sousa Meneses de Santana 
-- 202000024285

import Data.List 
import Data.Char 

type Nome = String
type Matricula = Int
type Usuario = (Matricula, Nome)
type Usuarios =[Usuario]
type Titulo = String
type Autor = String
type Livro = (Titulo, [Autor])
type Quant = Int
type Acervo = [(Livro, Quant)]
type Emprestimos = [(Matricula, [Livro])]


livrosEmprestados :: Emprestimos -> [Titulo]
livrosEmprestados [] = []
livrosEmprestados ((a,b):xs) = nub ([ x | (x,y)<-b] ++ livrosEmprestados xs)


livros :: Autor -> Acervo -> [Titulo]
livros autor acervo = map fst (map fst lista)
   where lista = filter (\((a,b),c)-> elem autor b) acervo


naoEmprestaram :: Emprestimos -> Usuarios -> Usuarios
naoEmprestaram emprestimos usuarios = sortOn fst (filter (\(a,b)-> (notElem a lista)) usuarios) 
  where lista = [x | (x,y)<-emprestimos] 

maiorMenor :: Acervo -> ([Titulo],[Titulo])
maiorMenor acervo = ((titulos maior acervo),(titulos menor acervo))
  where maior = foldr1 max (map (\(a,b)-> b) acervo)
        menor = foldr1 min (map (\(a,b)-> b) acervo)
        titulos :: Quant -> Acervo -> [Titulo]
        titulos quant acervo = map fst (map fst (filter (\(a,b)-> b == quant) acervo))

sum (dobroLista (xs++ys)) = 2*(sum xs) + 2*(sum ys)

[] ++ zs = zs (++.1)
(w:ws) ++ zs = w:(ws++zs) (++.2)
dobroLista [] = [] (dL.1)
dobroLista (w:ws) = 2*w: dobroLista ws (dL.2)
sum [] = 0 (s.1)
sum (w:ws) = w + sum ws (s.2)

caso base : xs = []

lado esquerdo : sum (dobroLista (xs++ys)) 
sum (dobroLista ([]++ys)) <++.1>
sum (dobroLista ys) 

lado direito : 2*(sum xs) + 2*(sum ys)
2*(sum []) + 2*(sum ys)   <s.1>
2*0 + 2*(sum ys)  <álgebra>
2*(sum ys) 

HI : a propriedade vale para uma lista xs de k elementos:
     sum(dobroLista ys) = 2*(sum ys)

Case geral : prova para uma lista (x:xs) de k+1 elementos  

sum (dobroLista ((x:xs)++ys)) = 2*(sum (x:xs)) + 2*(sum ys)

sum (dobroLista ((x:xs)++ys)) <++.2>
sum (dobroLista (x:(xs++ys))) <dL.2>
sum (2*x:dobroLista (xs++ys)) <s.2>
2*x + sum (dobroLista (xs++ys)) <HI>
2*x + 2*(sum (xs++ys)) <s.2>
2*(sum (x:(xs++ys))) <++.2>
2*(sum ((x:xs)++ys) <a.2>
2*(sum (x:xs) + sum ys) <álgebra>
2*(sum (x:xs)) + 2*(sum ys)


provar que : sum(xs++ys) = sum xs + sum ys -> (a.2) 
caso base: xs = []
lado esquerdo : sum([]++ys) <++.1>
sum ys 
lado direito : sum [] + sum ys <s.1>
0 + sum ys <álgebra>
sum ys 
HI.2 : sum(xs++ys) = sum xs + sum ys
caso geral : sum ((x:xs)++ys) = sum (x:xs) + sum ys 
sum ((x:xs)++ys) <HI.2>
sum (x:xs) + sum ys 



{-sum (dobroLista ((x:xs)++ys)) <HI>
2*(sum ((x:xs)++ys))  <++ .assoc>
2*(sum (x:xs) + sum (ys))  
2*(sum(x:xs)) + 2*(sum ys)-}
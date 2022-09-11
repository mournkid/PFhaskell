-- Camille Sousa Meneses de Santana 
--202000024285

{-Dada uma lista de inteiros, elaborar uma função recursiva temNeg para identificar
se algum elemento da lista é negativo. Ex:
temNeg [] retorna False
temNeg [2,3,4] retorna False
temNeg [2,-1,0] retorna True-}

temNeg :: [Int] -> Bool 
temNeg [] = False
temNeg (x:xs) = x < 0 || temNeg xs

prefixo1 :: String -> String -> Bool
prefixo1 [] _ = True
prefixo1 _ [] = False
prefixo1 (x:xs) (y:ys) 
  |x == y && prefixo1 xs ys = True
  |otherwise = False


type NomeAluno = String
type Nota = Float
type Notas = [Nota]
type Aluno = (NomeAluno, Notas)
type Alunos = [Aluno]
type Media = Float
type AlunoMed = (NomeAluno, Media)
type AlunosMedias = [AlunoMed]
type Disciplina = String
type DiscAlunosNotas = (Disciplina, Alunos)
type DiscAlunosMedias = (Disciplina, AlunosMedias)
type DisciplinasOfertadas = [DiscAlunosMedias]
type Historico = (NomeAluno, [(Disciplina, Media)])


calculaMedia :: DiscAlunosNotas -> DiscAlunosMedias
calculaMedia ("",[]) = ("",[])
calculaMedia (x,[]) = (x,[])
calculaMedia (disciplina,((nomeAluno,notas):ys)) = (disciplina,recursiva ((nomeAluno,notas):ys))
       where media :: Notas -> Media
             media notas = sum notas/fromIntegral (length notas)  
             recursiva :: Alunos -> AlunosMedias
             recursiva [] = [] 
             recursiva ((nomeAluno,notas):ys) = (nomeAluno,media notas) : recursiva ys 


{-Elabore uma função recursiva para dado um elemento do tipo
DisciplinasOfertadas e o nome de um aluno, gerar o histórico deste aluno do
tipo Historico.
geraHistorico [] “” retorna (“”,[])
geraHistorico [(“Calculo”, [(“Maria”,8.0), (“Ana”, 9.0), (“Ivo”,
4.5)]), (“Vetores”, [(“Ana”, 7.5), (“Pedro”, 5.0)])] “Ivo”
retorna (“Ivo”, [(“Calculo”, 4.5)])
geraHistorico [(“Calculo”, [(“Maria”,8.0), (“Ana”, 9.0), (“Ivo,
4.5”)]), (“Vetores”, [(“Ana”, 7.5), (“Pedro”, 5.0)])] “Paulo”
retorna (“Paulo”, [])
geraHistorico [(“Calculo”, [(“Maria”,8.0), (“Ana”, 9.0), (“Ivo,
4.5”)]), (“Vetores”, [(“Ana”, 7.5), (“Pedro”, 5.0)])] “Ana”
retorna (“Ana”, [(“Calculo”, 9.0),(“Vetores”, 7.5)])
-}

{-geraHistorico :: DisciplinasOfertadas -> NomeAluno -> Historico
geraHistorico [] "" = ("",[])
geraHistorico ((disciplina,((nome,media):ys):xs):zs) nomeAluno 
  |nome == nomeAluno = (nomeAluno,medias ((disciplina,(media ((nome,media):ys) nomeAluno))) (nomeAluno))
  |otherwise = geraHistorico zs nomeAluno
    where medias :: DisciplinasOfertadas -> NomeAluno ->  [(Disciplina, Media)]
          medias ((disciplina,((nome,media):ys):xs):zs) nomeAluno
            |nome == nomeAluno = [(disciplina,media):medias zs nomeAluno]
            |otherwise = medias zs nomeAluno -}

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


            
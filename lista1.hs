type CadastroSUS = [Cidadao]
type CPF = Integer
type Nome = String
type Genero = Char
type Dia = Int
type Mes = Int
type Ano = Int
type Data = (Dia, Mes, Ano)
type DataNasc = Data
type Endereco = String
type Municipio = String
type Estado = String
type Telefone = String
type Email = String
type Cidadao = (CPF, Nome, Genero, DataNasc, Endereco, Municipio,Estado, Telefone, Email)
type IdadeInicial = Int
type IdadeFinal = Int
type FaixaIdade = (IdadeInicial, IdadeFinal)
type Quantidade = Int
type QuantidadeFormatada = String
type LinhaFormatada = String
type Vacinados = [Vacinado]
type Vacina = String
type TipoDose = Int
type Dose = (Vacina, Data)
type Doses = [Dose]
type Vacinado = (CPF, Doses)


checaCPF :: CPF -> CadastroSUS -> Bool
checaCPF cpf cadastro 
  | or [a == cpf | (a,b,c,d,e,f,g,h,i) <- cadastro] == False = False 
  | otherwise = True 


adicionaSUS :: Cidadao -> CadastroSUS -> CadastroSUS
adicionaSUS (a,b,c,d,e,f,g,h,i) cadastro 
  | (checaCPF a cadastro) == True = (a,b,c,d,e,f,g,h,i) : meuCadastro
  | otherwise = error "esse cadastro ja existe"

atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS
atualizaEndSUS cpf cadastro endereco = remove ++ atualiza 
  where atualiza = [(a,b,c,d,endereco,f,g,h,i) | (a,b,c,d,e,f,g,h,i)<-cadastro, a==cpf]
        remove = [(a,b,c,d,e,f,g,h,i) | (a,b,c,d,e,f,g,h,i)<-cadastro, a/=cpf]

atualizaTelSUS :: CPF -> CadastroSUS -> Telefone -> CadastroSUS
atualizaTelSUS cpf cadastro tel = [atualiza x | x<-cadastro]
   where atualiza :: Cidadao -> Cidadao
         atualiza (a,b,c,d,e,f,g,h,i) 
           | a == cpf = (a,b,c,d,e,f,g,tel,i)
           |otherwise = (a,b,c,d,e,f,g,h,i)

removeSUS :: CPF -> CadastroSUS -> CadastroSUS
removeSUS cpf cadastro 
   | [(a,b,c,d,e,f,g,h,i) | (a,b,c,d,e,f,g,h,i) <- cadastro, a == cpf ] == [] = error "cidadao nao pertence ao cadastro"
   | otherwise = [(a,b,c,d,e,f,g,h,i) | (a,b,c,d,e,f,g,h,i) <- cadastro, a /= cpf ]  


cidadaosPorMunicipio :: CadastroSUS -> Municipio -> Quantidade
cidadaosPorMunicipio cadastro municipio = sum [ 1 | (a,b,c,d,e,f,g,h,i)<-cadastro, f == municipio]

cidadaosPorEstado :: CadastroSUS -> Estado -> Quantidade
cidadaosPorEstado cadastro estado = sum [ 1 | (a,b,c,d,e,f,g,h,i)<-cadastro, g == estado]

{-cidadaosPorMunicipioIdade :: CadastroSUS -> Municipio-> FaixaIdade -> Quantidade
cidadaosPorMunicipioIdade cadastro municipio (inicial,final) = sum [ 1 | (a,b,c,d,e,f,g,h,i)<-cadastro, f == municipio,(idade d) >= inicial && (idade d) <= final]
  where idade :: DataNasc -> Int
        idade (dia,mes,ano) = 2021 - ano -}

cidadaosPorEstadoIdade :: CadastroSUS -> Estado -> FaixaIdade -> Quantidade
cidadaosPorEstadoIdade cadastro estado (inicial,final) = sum [ 1 | (a,b,c,d,e,f,g,h,i)<-cadastro, g == estado,(idade d) >= inicial && (idade d) <= final]
  where idade :: DataNasc -> Int
        idade (dia,mes,ano) = 2021 - ano

{-geraListaMunicipioFaixas :: CadastroSUS -> Municipio -> [FaixaIdade] -> [(FaixaIdade, Quantidade)]
geraListaMunicipioFaixas cadastro municipio idade = [(faixa,cidadaosPorMunicipioIdade cadastro municipio faixa) | faixa<-idade]-}

geraListaEstadoFaixas :: CadastroSUS -> Estado -> [FaixaIdade] -> [(FaixaIdade, Quantidade)]
geraListaEstadoFaixas cadastro estado idade = [(faixa,cidadaosPorEstadoIdade cadastro estado faixa) | faixa<-idade]

formataQuant :: Quantidade -> QuantidadeFormatada
formataQuant qnt 
  | qnt >= 0 && qnt <= 9 = "      " ++ show qnt 
  | qnt >= 10 && qnt <= 99 = "     " ++ show qnt 
  | qnt >= 100 && qnt <= 999 = "    " ++ show qnt 
  | qnt >= 1000 && qnt <= 9999 = "   " ++ show qnt
  | qnt >= 10000 && qnt <= 99999 = "  " ++ show qnt 
  | qnt >= 100000 && qnt <= 999999 = " " ++ show qnt
  | otherwise = show qnt 

formataUmaLinha :: (FaixaIdade, Quantidade)-> LinhaFormatada 
formataUmaLinha ((x,y),b) = show x ++ "-" ++ show y ++ "               " ++ formataQuant b 

type LinhasFormatadas = String
formataLinhas :: [(FaixaIdade, Quantidade)] -> LinhasFormatadas
formataLinhas lista = concat [formataUmaLinha x ++ "\n"| x<-lista]

type TotalFormatado = String
formataTotal :: [(FaixaIdade,Quantidade)] -> TotalFormatado
formataTotal lista = "TOTAL" ++ "               " ++ formataQuant total 
   where total = sum [ b  | (a,b)<- lista]
{-
listaMunicipioFaixas :: CadastroSUS -> Municipio -> [FaixaIdade] -> IO()
listaMunicipioFaixas cadastro municipio faixa = putStr ("MUNICIPIO: " ++ municipio ++ "\n\n" ++ formataLinhas lista ++ "\n\n" ++ formataTotal lista ++ "\n")
   where lista = geraListaMunicipioFaixas cadastro municipio faixa-}

listaEstadoFaixas :: CadastroSUS -> Estado-> [FaixaIdade] -> IO()
listaEstadoFaixas cadastro estado faixa = putStr ("ESTADO: " ++ estado ++ "\n\n" ++ formataLinhas lista ++ "\n\n" ++ formataTotal lista ++ "\n")
  where lista = geraListaEstadoFaixas cadastro estado faixa 

{-aplicaPrimDose:: CPF -> CadastroSUS -> FaixaIdade -> Municipio -> Vacina -> Data -> Vacinados -> Vacinados
aplicaPrimDose cpf -}

quantidadeDoseMun :: Vacinados -> TipoDose -> Municipio -> CadastroSUS -> Quantidade
quantidadeDoseMun cadastroVacinados tDose mun cadastro = tipo
     where tipo = length [ doses |(cpf,doses)<- cadastroVacinados,(a,b,c,d,e,f,g,h,i)<- cadastro, length doses == tDose, a==cpf && f == mun]

pessoasVacinadas :: Vacinados
pessoasVacinadas = [(87717347115,[("Pfizer",(20,9,2021))]),(07782377536,[("AstraZeneca",(20,10,2021))])] 

quantidadeMunVacDose :: Vacinados -> Municipio -> Vacina -> TipoDose -> CadastroSUS -> Quantidade -- exibe a quantidade de pessoas errada, porque se tiver na lista pessoas com duas doses e colocar o tipo de dose menor que aquele, vai contar do mesmo jeito e o enunciado pede a quantidade por tipo de dose
quantidadeMunVacDose vacinados municipio vacina tipoDose cadastro = sum [1 | (cpf,doses)<-lista, length doses == tipoDose, fst (head doses) == vacina]
   where cpfcidadao :: CadastroSUS -> CPF -> Municipio
         cpfcidadao k j = head [ f |(a,b,c,d,e,f,g,h,i)<-k, a==j]
         lista = [ (cpf,doses) |(cpf,doses) <-vacinados, (a,b,c,d,e,f,g,h,i)<-cadastro, cpf == a, f == municipio]


obterMunicipio2 :: CPF -> CadastroSUS -> Municipio
obterMunicipio2 cpf cadastro = obterMunicipio (localizarCidadao cpf cadastro)

obterMunicipio :: Cidadao -> Municipio
obterMunicipio (_,_,_,_,_,municipio,_,_,_) = municipio

localizarCidadao :: CPF -> CadastroSUS -> Cidadao
localizarCidadao cpf cadastro = head [x | x <- cadastro, obterCPF x == cpf]

obterCPF :: Cidadao -> CPF
obterCPF (cpf,_,_,_,_,_,_,_,_) = cpf

dosesJaTomadas :: CPF -> Vacinados -> TipoDose
dosesJaTomadas cpf vacinados = length (snd(head(checaCPFvacinado1 cpf vacinados)))

checaCPFvacinado1 :: CPF -> Vacinados -> Vacinados
checaCPFvacinado1 cpf vacinados = [numeroDeCPF | numeroDeCPF <- vacinados, obterCPFvacinado numeroDeCPF == cpf]

obterCPFvacinado :: Vacinado -> CPF
obterCPFvacinado (cpf,_) = cpf

obterVacina :: CPF -> Vacinados -> Vacina
obterVacina cpf vacinados = fst(head(snd(head(checaCPFvacinado1 cpf vacinados))))

listaDeVacinados :: Vacinados
listaDeVacinados = [(26716347665, [("CoronaVac", (22,09,2021)), ("CoronaVac", (20,10,2021))]), (56789432109, [("Pfizer", (10,07,2021))]), 
                   (87717347115, [("AstraZeneca", (14,08,2021)),("AstraZeneca",(13,10,2021))]), (07782377536, [("Janssen", (30,06,2021))])
                   ]

meuCadastro :: CadastroSUS
meuCadastro = [(26716347665, "Paulo Souza", 'M', (11,10,1996),"Rua A, 202","Muribeca", "SE", "999997000", "psouza@gmail.com"),
               (87717347115, "Ana Reis",'F', (5,4,1970), "Rua B, 304","Aracaju", "SE", "999826004","areis@gmail.com"),
               (07782377536,"camille",'F',(02,11,2002),"rua frei","Aracaju","SE","998597816","mille_sousa@outlook.com")
              ]

cidadaosPorMunicipioIdade :: CadastroSUS -> Municipio -> Data -> FaixaIdade -> Quantidade
cidadaosPorMunicipioIdade [] _ _ _  = 0
cidadaosPorMunicipioIdade (x:xs) mun dat idadeFaixa
      | mun == pegaMun x && fst idadeFaixa <= (pegaIdade2 dat (pegaIdade1 x)) && snd idadeFaixa >= (pegaIdade2 dat (pegaIdade1 x)) = 1 + cidadaosPorMunicipioIdade xs mun dat idadeFaixa
      | otherwise = cidadaosPorMunicipioIdade xs mun dat idadeFaixa 

geraListaMunicipioFaixas :: CadastroSUS -> Municipio -> Data -> [FaixaIdade] -> [(FaixaIdade, Quantidade)]
geraListaMunicipioFaixas [] _ _ _ = []
geraListaMunicipioFaixas _ _ _ [] = []
geraListaMunicipioFaixas (x:xs) mun dat (i:is) = [(i,cidadaosPorMunicipioIdade (x:xs) mun dat i)] ++ geraListaMunicipioFaixas (x:xs) mun dat is   
            

pegaMun :: Cidadao -> Municipio
pegaMun (_,_,_,_,_,m,_,_,_) = m

pegaAno :: DataNasc -> Ano
pegaAno (_,_,a) = a

pegaIdade1 :: Cidadao -> DataNasc
pegaIdade1 (_,_,_,i,_,_,_,_,_) = i

pegaIdade2 :: Data -> DataNasc -> Int
pegaIdade2 (dia, mes, ano) (diaN, mesN, anoN)
     | dia >= diaN && mes >= mesN = pegaAno (dia, mes, ano) - pegaAno (diaN, mesN, anoN)
     | otherwise = (pegaAno (dia, mes, ano) - pegaAno (diaN, mesN, anoN)) - 1 

vacinando :: CadastroSUS -> Data -> Int -> Municipio -> Int -> (Int,Quantidade) -> (Int,Int)
vacinando [] _ _ _ _ _ = (0,0)
vacinando (x:xs) dat ultIdade mun qtdVacina acc
    | comparaMun mun (x:xs) && pegaIdade2 dat (pegaIdade1 x) < ultIdade = (pred ultIdade,acumulador (x:xs) dat qtdVacina acc)
--    | comparaMun mun (x:xs) && pegaIdade2 dat (pegaIdade1 x) < ultIdade && qtdVacina >= length xs = qtdVacina - (quantidadePessoas (x:xs) dat ultIdade mun)
    | otherwise = vacinando xs dat ultIdade mun qtdVacina acc
     where quantidadePessoas :: CadastroSUS -> Data -> Int -> Municipio -> Int
           quantidadePessoas [] _ _ _ = 0
           quantidadePessoas (x:xs) dat ultIdade mun
               | comparaMun mun (x:xs) && pegaIdade2 dat (pegaIdade1 x) < ultIdade = 1 + quantidadePessoas xs dat ultIdade mun
               | otherwise = quantidadePessoas xs dat ultIdade mun
     
           pegaPessoas :: [(Int,Int)] -> Int  
           pegaPessoas [(_,pes)] = pes    

acumulador :: CadastroSUS -> Data -> Int -> (Int,Quantidade) -> Int
acumulador [] _ _ acc = fst acc
acumulador (x:xs) dat qtdVac acc 
  |snd (head (pessoasPorIdade dat (x:xs))) + snd acc > qtdVac = fst acc 
  |otherwise = acumulador (remover (x:xs) (head (pessoasPorIdade dat (x:xs)))) dat qtdVac ( fst (head (pessoasPorIdade dat (x:xs))), snd (head (pessoasPorIdade dat (x:xs))) + snd acc) 
    where remover :: CadastroSUS -> (Int,Quantidade) -> CadastroSUS
          remover [] _ = []
          remover (x:xs) y 
            |fst y == pegaIdade2 dat (pegaIdade1 x) = remover xs y 
            |otherwise = x: remover xs y      

pessoasVacinadas1 :: Vacinados
pessoasVacinadas1 = [(73984201325,[("AstraZeneca",(20,9,2021))]),(53471688765,[("AstraZeneca",(20,10,2021)),("AstraZeneca",(25,11,2021))]),
                    (12345678901,[("CoronaVac",(25,10,2021))]),(48912893119,[("Janssen",(25,10,2021)),("Janssen",(25,10,2021))]) 
                   ]

meuCadastro1 :: CadastroSUS
meuCadastro1  = [(73984201325, "Jose Adelson Lima Santos Junior", 'M', (3,6,2002),"Rua A, 255","Aracaju","SE","988033716","js.2002@gmail.com"),(53471688765, "Maria Silva", 'F', (21,12,1984),"Rua A, 202", "Aracaju", "SE", "999880300", "msilva@gmail.com"), (12345678901, "Martinez Salgado", 'X', (5,4,2005), "Rua Alameda,411","Aracaju","SE","029319341","martinez.01@outlook.com"),(48912893119, "Alabasta Cortez", 'F', (5,4,2005), "Rua Alameda,123","Aracaju","SE","0123910102","alabastacortez@gmail.com")]

ordenarAux :: (Int,Quantidade) -> [(Int,Quantidade)] -> [(Int,Quantidade)] 
ordenarAux _ [] = []
ordenarAux n (x:xs) 
    |  n == x = xs
    | otherwise = x : ordenarAux n xs
 
ordenar :: [(Int,Quantidade)] -> [(Int,Quantidade)] 
ordenar [] = []
ordenar  xs = y : ordenar (ordenarAux y xs)
    where y = pegaMaior xs

maior :: (Int,Int) -> (Int,Int) -> (Int,Int)
maior (a,b) (c,d) = if fst (a,b) >= fst (c,d) then (a,b) else (c,d)

pegaMaior :: [(Int,Quantidade)] -> (Int,Quantidade)
pegaMaior [x] = x
pegaMaior (x:xs) = maior x (pegaMaior xs)

comparaMun :: Municipio -> CadastroSUS -> Bool
comparaMun _  [] = False
comparaMun mun (x:xs)
   | pegaMun x == mun = True
   | otherwise = comparaMun mun xs

pessoasIdade ::  Data ->  CadastroSUS -> [Int]
pessoasIdade _  [] = []
pessoasIdade dat (x:xs) = (pegaIdade2 dat (pegaIdade1 x)) : pessoasIdade dat xs

pessoasPorIdade :: Data -> CadastroSUS -> [(Int,Quantidade)]
pessoasPorIdade dat cadastro = ordenar (qtdLista (nub (pessoasIdade dat cadastro)) (pessoasIdade dat cadastro))

qtdLista :: [Int] -> [Int] -> [(Int,Int)]
qtdLista _ [] = []
qtdLista [] _ = []
qtdLista (x:xs) ys = [(x,elemNum ys x)] ++ qtdLista xs ys
      
elemNum :: [Int] -> Int -> Int
elemNum [] _ = 0
elemNum (x:xs) a 
        |x == a = 1 + elemNum xs a
        |otherwise = elemNum xs a 

nub :: [Int] -> [Int]
nub l = nub' l []
      where
            nub' [] _      = []
            nub' (x:xs) ls
                |elem x ls = nub' xs ls
                |otherwise = x : nub' xs (x:ls) 

checaDose :: CPF -> Vacinados -> Bool
checaDose _ [] = False
checaDose cpf ((a,b):xs) 
   | a == cpf && length b == 1 = True
   | otherwise = checaDose cpf xs


     
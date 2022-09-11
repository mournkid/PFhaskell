type Municipio = String
type Populacao = Int
type PopMun = (Municipio, Populacao)
data Arv a b  = NoNulo | No (a,b) (Arv a b) (Arv a b) deriving (Show,Eq,Ord,Read)
cadastro:: Arv a b
cadastro = No ("Itabaiana", 100000) (No ("Capela", 35000) (No ("Aracaju", 675000) NoNulo NoNulo) (No ("Estancia", 70000) NoNulo NoNulo)) (No ("São Cristovao", 92100) (No ("Lagarto", 107000) NoNulo NoNulo) (No ("Siriri", 10000) NoNulo NoNulo))

{-coleta ::  Municipio -> Arv a b -> Populacao
coleta h NoNulo = error "Não há informação"
coleta h (No (i,j) arvE arvD) |h == i = j     
                              |h < i = coleta h arvE
                              |otherwise = coleta h arvD
-}
coletaNo :: Ord a => a -> Arv a b  -> a
coletaNo x NoNulo = error "não existe"
coletaNo x (No (a,b) tesq tdir)
  | x == a = b 
  | x < a = coletaNo x tesq
  | otherwise = coletaNo x tdir 
-- Camille Sousa Meneses de Santana 
--202000024285
module ABB (Arv(..), arvVazia, ehVazia, ehNoNulo, arvEsq, arvDir,infoNo,coletaNo,removeNo,insereNo,atualizaNo,mapArv) where

data Arv a b c d e = NoNulo | No (a,b,c,d,e) (Arv a b c d e) (Arv a b c d e) deriving (Show,Eq,Ord,Read)

arvVazia :: Arv a b c d e 
arvVazia = NoNulo 

ehVazia :: Arv a b c d e -> Bool 
ehVazia (NoNulo) = True 
ehVazia _ = False

ehNoNulo :: Arv a b c d e -> Bool 
ehNoNulo NoNulo = True
ehNoNulo _ = False

arvEsq :: Arv a b c d e -> Arv a b c d e
arvEsq (NoNulo) = error "nao tem arvore esquerda"
arvEsq (No _ tesq _) = tesq

arvDir :: Arv a b c d e -> Arv a b c d e
arvDir (NoNulo) = error "nao tem arvore direita"
arvDir (No _ _ tdir) = tdir 

infoNo :: Arv a b c d e -> (a,b,c,d,e) 
infoNo NoNulo = error "arvore vazia"
infoNo (No x _ _) = x

insereNo ::  Ord a => (a,b,c,d,e) -> Arv a b c d e -> Arv a b c d e
insereNo x NoNulo = (No x NoNulo NoNulo)
insereNo (m,n,o,p,q) x@(No (a,b,c,d,e) tesq tdir)
  | aux (m,n,o,p,q) x = error "ja existe"
  | m < a = No (a,b,c,d,e) (insereNo (m,n,o,p,q) tesq) tdir
  | otherwise = No (a,b,c,d,e) tesq (insereNo (m,n,o,p,q) tdir) 
    where aux :: Ord a => (a,b,c,d,e) -> Arv a b c d e -> Bool 
          aux x NoNulo = False
          aux (m,n,o,p,q) (No (a,b,c,d,e) tesq tdir) = m == a || aux (m,n,o,p,q) tesq || aux (m,n,o,p,q) tdir 


removeNo :: Ord a => a -> Arv a b c d e -> Arv a b c d e
removeNo x NoNulo = error "nao há elementos a remover"
removeNo x (No (a,b,c,d,e) tesq tdir)
  | x < a = No (a,b,c,d,e) (removeNo x tesq) tdir
  | x > a = No (a,b,c,d,e) tesq (removeNo x tdir)
  | ehNoNulo tdir = tesq
  | ehNoNulo tesq = tdir
  | otherwise = una tesq tdir
    where una :: Ord a =>  Arv a b c d e -> Arv a b c d e -> Arv a b c d e
          una tesq tdir = No mini tesq novaArv
            where (Just mini) = minArv tdir
                  novaArv = removeNo (aux) tdir
                  (Just aux) = info (minArv tdir)
          minArv :: Ord a => Arv a b c d e -> Maybe (a,b,c,d,e)
          minArv t
           | ehNoNulo t = Nothing
           | ehNoNulo (arvEsq t) = Just (infoNo t)
           | otherwise = minArv (arvEsq t)      
          info :: Maybe (a,b,c,d,e) -> Maybe a 
          info (Just (a,b,c,d,e)) = Just a 
          info Nothing = Nothing

mapArv :: ((a,b,c,d,e) -> (m,n,o,p,q)) -> Arv a b c d e -> Arv m n o p q 
mapArv f NoNulo = NoNulo 
mapArv f (No x tesq tdir) = No (f x) (mapArv f tesq) (mapArv f tdir) 

coletaNo :: Ord a => a -> Arv a b c d e -> (a,b,c,d,e)
coletaNo x NoNulo = error "não existe"
coletaNo x (No (a,b,c,d,e) tesq tdir)
  | x == a = (a,b,c,d,e)
  | x < a = coletaNo x tesq
  | otherwise = coletaNo x tdir 
 
atualizaNo :: Ord a => (a,b,c,d,e) -> Arv a b c d e -> Arv a b c d e    
atualizaNo (m,n,o,p,q) NoNulo = NoNulo
atualizaNo (m,n,o,p,q) (No (a,b,c,d,e) tesq tdir)
  | m == a = No (a,n,o,p,q) tesq tdir
  | m < a = No (a,b,c,d,e) (atualizaNo (m,n,o,p,q) tesq) tdir
  | otherwise = No (a,b,c,d,e) tesq (atualizaNo (m,n,o,p,q) tdir)


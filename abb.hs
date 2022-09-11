module ABB1 (Arv, arvVazia, ehVazia, ehNoNulo, arvEsq, arvDir, infoNo, consulta, insere, remove) where

data Arv a = NoNulo | No a (Arv a) (Arv a) deriving (Show,Read,Eq,Ord)

arvVazia :: Arv a
arvVazia = NoNulo

ehVazia :: Arv a -> Bool
ehVazia (NoNulo) = True
ehVazia _ = False

ehNoNulo :: Arv a -> Bool
ehNoNulo NoNulo = True
ehNoNulo _ = False

arvEsq :: Arv a -> Arv a
arvEsq (NoNulo) = error "nao tem subarvore esquerda"
arvEsq (No _ tesq _) = tesq

arvDir :: Arv a -> Arv a
arvDir (NoNulo) = error "nao tem subarvore direita"
arvDir (No _ _ tdir) = tdir

infoNo :: Arv a -> a
infoNo NoNulo = error "arvore vazia"
infoNo (No x _ _ ) = x

consulta :: Ord a => a -> Arv a -> Bool
consulta _ NoNulo = False
consulta x (No y tesq tdir)
  | x == y = True
  | x < y = consulta x tesq
  | otherwise = consulta x tdir

insere :: Ord a => a -> Arv a -> Arv a
insere x NoNulo = (No x NoNulo NoNulo)
insere x (No y tesq tdir)
  | x == y = (No y tesq tdir)
  | x < y = No y (insere x tesq) tdir
  | otherwise = No y tesq (insere x tdir)

remove :: Ord a => a -> Arv a -> Arv a
remove x NoNulo = error "nao há elementos a remover"
remove x (No y tesq tdir)
  | x < y = No y (remove x tesq) tdir
  | x > y = No y tesq (remove x tdir)
  | ehNoNulo tdir = tesq
  | ehNoNulo tesq = tdir
  | otherwise = una tesq tdir

una :: Ord a => Arv a -> Arv a -> Arv a
una tesq tdir = No mini tesq novaArv
  where (Just1 mini) = minArv tdir
        novaArv = remove mini tdir
              -- EXPLICAR ESSAS DUAS 
minArv :: Ord a => Arv a -> Maybe1 a
minArv t
  | ehNoNulo t = Nothing1
  | ehNoNulo (arvEsq t) = Just1 (infoNo t)
  | otherwise = minArv (arvEsq t)


data Maybe1 a = Nothing1 | Just1 a deriving (Eq, Ord, Show, Read)
























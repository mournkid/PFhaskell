module Set (Conjunto,vazio,doisIguais,elemento,uniao,intersecao,subconjunto,mapConj,foldrConj) where 

import Data.List 
import Data.Char

data Conjunto a = Conjunto [a] deriving (Show,Ord,Eq)

vazio :: Conjunto a 
vazio = Conjunto [] 

doisIguais :: Ord a => Conjunto a -> Conjunto a -> Bool 
doisIguais (Conjunto ys) (Conjunto xs) = sort ((nub ys) `union` (nub xs)) == sort (nub xs)  
     
elemento :: Eq a => a -> Conjunto a -> Bool
elemento x (Conjunto ys) = elem x ys 

uniao :: Ord a => Conjunto a -> Conjunto a -> Conjunto a 
uniao (Conjunto xs) (Conjunto ys) = Conjunto (sort (nub (aux xs ys)))
  where aux :: Ord a => [a] -> [a] -> [a]
        aux xs ys = xs `union` ys   

intersecao :: Ord a => Conjunto a -> Conjunto a -> Conjunto a
intersecao (Conjunto xs) (Conjunto ys) = Conjunto (sort (nub (xs `intersect` ys)))

{-⦁	diferença simétrica de conjuntos
⦁	construção de um conjunto a partir de uma lista
⦁	map e foldr para conjuntos
⦁	cardinalidade de conjuntos
⦁	impressão de um conjunto com a notação convencional de {}-}

subconjunto :: Ord a => Conjunto a -> Conjunto a -> Bool
subconjunto _ (Conjunto []) = True 
subconjunto (Conjunto []) _ = True
subconjunto (Conjunto xs) (Conjunto ys) = isSubsequenceOf (sort (nub xs)) (sort (nub ys)) 

mapConj :: (a->b) -> Conjunto a -> Conjunto b 
mapConj f (Conjunto (x:xs)) = Conjunto (aux f (x:xs))
  where aux :: (a->b) -> [a] -> [b]
        aux f [] = []
        aux f (x:xs) = f x : aux f xs 

foldrConj :: (a->b->b) -> b -> Conjunto a -> b 
foldrConj f b (Conjunto []) = b 
foldrConj f b (Conjunto (x:xs)) = f x (foldrConj f b (Conjunto xs))

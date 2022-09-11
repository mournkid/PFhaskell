module Fila (Fila, filaVazia, ehVazia, insere, remove) where 

newtype Fila a = Fil [a] deriving (Show,Read,Eq,Ord)

filaVazia :: Fila a
filaVazia = Fil []

ehVazia :: Fila a -> Bool
ehVazia (Fil []) = True
ehVazia _ = False

insere :: a -> Fila a -> Fila a
insere x (Fil xs) = Fil (xs ++ [x])

remove :: Fila a -> (a, Fila a)
remove (Fil xs)  
  | not (ehVazia (Fil xs)) = (head xs, Fil (tail xs))
  | otherwise = error "nao ha elementos a remover"

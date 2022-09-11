-- novos tipos 
-- define construtores de valores 
data Temp = Frio | Quente 
data Semana = Dom | Seg | Ter | Qua | Qui | Sex | Sab
data Estacao = Primavera | Verao | Outono | Inverno


--É possível derivar automaticamente instâncias das classes de tipos pré-definidas em Haskell para o novo tipo criado(Read,Show,Ord, Eq)

data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show) 

--ghci> map (Circle 10 20) [4,5,6,6] -> [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]   

-- Registros

data Person = Person String String Int Float String String deriving (Show)  

firstName :: Person -> String  
firstName (Person firstname _ _ _ _ _) = firstname  
  
lastName :: Person -> String  
lastName (Person _ lastname _ _ _ _) = lastname  
  
age :: Person -> Int  
age (Person _ _ age _ _ _) = age  
  
height :: Person -> Float  
height (Person _ _ _ height _ _) = height  
  
phoneNumber :: Person -> String  
phoneNumber (Person _ _ _ _ number _) = number  
  
flavor :: Person -> String  
flavor (Person _ _ _ _ _ flavor) = flavor

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)


data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)  

ghci> Car {company="Ford", model="Mustang", year=1967}  
Car {company = "Ford", model = "Mustang", year = 1967}  


função Car {company = a, model = b, year = c} = a b c 

-- cria funções que vem do tipo, como:

--flavor :: Person -> String 

--firstName :: Person -> String 


data Maybe a = Nothing | Just a



-- tipos algébricos recursivos 

data Arv = NoNulo
	    |No Integer Arv Arv  deriving (Show,Eq,Read,Ord) 







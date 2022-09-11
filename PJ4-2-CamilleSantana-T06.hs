-- Camille Sousa Meneses de Santana 
--202000024285

import ABB 
import Data.List 

type CodCliente = Int
type NomeCliente = String
type CategCliente = Char
type ConsumoAnual = Int 
type Cliente = (CodCliente,NomeCliente,CategCliente,MesAniversario,ConsumoAnual) 
type Compra = Int 
type Preco = Int
type ProdTotalizado = (CodProd, NomeProd, Quant, Preco)
type PedidoTotalizado = [ProdTotalizado]
type Desconto = Int
type PrecoFinal = Int
type Totalizacao = (Preco,Desconto,PrecoFinal)
type CodProd = Int
type NomeProd = String
type PrecoProd = Int 
type Quant = Int 
type SolCliente = (CodProd,NomeProd,Quant)
type PedidoCliente = [SolCliente]
type Produto = (CodProd,NomeProd,PrecoProd)
type Menu = [Produto]

data MesAniversario = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro deriving (Show,Read,Eq,Ord,Enum)

type Clientes = Arv CodCliente NomeCliente CategCliente MesAniversario ConsumoAnual 

fregueses :: Clientes
fregueses = No (10,"camille",'B',Novembro,95800) (No (7,"felipe",'E',Abril,650000) NoNulo (No (8,"alice",'A',Novembro,800) NoNulo NoNulo)) (No (15,"cecilia",'C',Agosto,67448) (No (12,"yann",'B',Junho,650000) NoNulo NoNulo) NoNulo) 

cardapio :: Menu
cardapio = [(1, "Acaraje",500), (2,"Hamburguer",700), (3,"Misto",450), (4,"Suco",300), (5,"Agua de coco",300), (6,"Acai", 500), (7,"Sorvete",400), (8,"Mousse",450)]


adicionaCliente :: Clientes -> CodCliente -> NomeCliente -> MesAniversario -> Clientes
adicionaCliente clientes cod nome mes = ABB.insereNo (cod,nome,'A',mes,0) clientes  

coletaCliente :: Clientes -> CodCliente -> Cliente
coletaCliente clientes cod = ABB.coletaNo cod clientes 

atualizaConsumo :: Clientes -> CodCliente -> Compra -> Clientes
atualizaConsumo clientes cod compra = ABB.atualizaNo (aux (coletaCliente clientes cod)) clientes
  where aux (a,b,c,d,e) = (a,b,c,d,e+compra) 

atualizaClientes :: Clientes -> Clientes
atualizaClientes clientes = ABB.mapArv atualiza clientes 
   where atualiza (a,b,c,d,e)
          |e < 50000 = (a,b,'A',d,0) 
          |e >=50000 && e<150000 = (a,b,'B',d,0) 
          |e >=150000 && e< 250000 = (a,b,'C',d,0) 
          |e >=250000 && e<350000 = (a,b,'D',d,0) 
          |otherwise = (a,b,'E',d,0)

totalPedido :: Clientes -> CodCliente -> PedidoTotalizado -> Totalizacao 
totalPedido clientes cod pedido = (preco pedido, (((preco pedido)*(desconto clientes cod)) `div` 100), ((preco pedido) - (((preco pedido)*(desconto clientes cod)) `div` 100)))
  where preco :: PedidoTotalizado -> PrecoFinal
        preco pedido = foldr (+) 0 (listaPreco pedido)
        listaPreco :: PedidoTotalizado -> [Preco]
        listaPreco pedido = map (\(a,b,c,d)->d) pedido 
        desconto :: Clientes -> CodCliente -> Desconto 
        desconto clientes cod 
          | aux (coletaCliente clientes cod) == 'A' = 0 
          | aux (coletaCliente clientes cod) == 'B' = 3
          | aux (coletaCliente clientes cod) == 'C' = 5 
          | aux (coletaCliente clientes cod) == 'D' = 10
          | otherwise = 15 
        aux (a,b,c,d,e) = c

geraConta :: CodCliente -> Clientes -> Menu -> PedidoCliente -> IO ()
geraConta codigo clientes menu pedido = putStr ("PEDIDO " ++ show codigo ++ "\n\n" ++ "COD   QTD  PRODUTO                  PRECO\n\n" ++ formataLinhas (geraPedidoImpressao pedido menu) ++ "\n\n" ++ formataTotal (totalPedido clientes codigo(geraPedidoImpressao pedido menu)) )

formataTotal:: Totalizacao-> String
formataTotal totalizacao = "\n         Total...................." ++ formataValor (devolvePreco totalizacao) ++"\n" ++ "         Desconto................." ++ formataValor (devolveDesconto totalizacao) ++ "\n" ++ "         A pagar.................." ++ formataValor (devolvePrecoFinal totalizacao) ++ "\n"
             where devolvePreco :: Totalizacao -> Int
                   devolvePreco (preco,desconto,precoFinal) = preco
                   devolveDesconto :: Totalizacao -> Int
                   devolveDesconto (preco,desconto,precoFinal) = desconto
                   devolvePrecoFinal :: Totalizacao -> Int
                   devolvePrecoFinal (preco,desconto,precoFinal) = precoFinal


formataLinha :: ProdTotalizado -> String
formataLinha prod =  (codigo.devolveCod) prod ++ "  " ++ (show.devolveQuant) prod ++ "  " ++ (nome.devolveNome) prod ++ (formataValor.devolvePreco) prod ++ "\n"
       where devolveCod :: ProdTotalizado -> Int
             devolveCod (cod,nome,quant,preco) = cod 
             devolveQuant :: ProdTotalizado -> Int
             devolveQuant (cod,nome,quant,preco) = quant
             devolveNome :: ProdTotalizado -> String
             devolveNome (cod,nome,quant,preco) = nome
             devolvePreco :: ProdTotalizado -> Int
             devolvePreco (cod,nome,quant,preco) = preco 
             nome :: NomeProd -> String
             nome nomeProd = nomeProd ++ replicate (25-(length nomeProd)) '.'   
             codigo :: CodProd -> String
             codigo x 
               |x < 10 = "   " ++ show x 
               |x >= 10 && x < 100 = "  " ++ show x 
               |x >= 100 && x < 1000 = " " ++ show x 
               |x >=1000 && x < 10000 = show x 
               |otherwise = error "contem mais que 4 digitos"

formataValor :: Int -> String 
formataValor x 
 |x < 10 = "....0.0" ++ show x
 |x >=10 && x < 100 = "....0." ++ show x
 |x >=100 && x < 1000 = "...." ++ show (x `div` 100) ++ "." ++ drop ((length (show x))-2) (show x)
 |x >=1000 && x < 10000 = "..." ++ show (x `div`100) ++ "." ++ drop ((length (show x))-2) (show x)
 |x >=10000 && x < 100000 = ".." ++ show (x `div`100) ++ "." ++ drop ((length (show x))-2) (show x)
 |otherwise = "." ++ show (x`div`100) ++ "." ++ drop ((length (show x))-2) (show x)

formataLinhas :: PedidoTotalizado -> String
formataLinhas pedido = concat (map formataLinha pedido)  

geraPedidoImpressao :: PedidoCliente -> Menu -> PedidoTotalizado
geraPedidoImpressao pedido menu = [(codProd,nomeProd,quant, (precoProd menu codProd)*quant) | (codProd,nomeProd,quant, preco)<-zipa pedido menu]                    
                      where precoProd :: Menu -> CodProd ->PrecoProd
                            precoProd menu cod = head [c | (a,b,c)<-menu, a==cod]
                            zipa :: PedidoCliente -> Menu -> PedidoTotalizado 
                            zipa pedido menu = [(codProd,nomeProd,quant, preco) |((codProd,nomeProd,quant),preco)<-zip pedido [c | (a,b,c)<-menu]]

transformaLista :: Clientes -> [Cliente] 
transformaLista NoNulo = []
transformaLista clientes = ordenaCliente (ABB.infoNo clientes : transformaLista (ABB.removeNo (aux (ABB.infoNo clientes)) clientes))
  where aux (a,b,c,d,e) = a
        ordenaCliente :: [Cliente] -> [Cliente]
        ordenaCliente clientes = foldr quickSort [] clientes
          where quickSort :: Cliente -> [Cliente] -> [Cliente]
                quickSort (a,b,c,d,e) clientes = filter (\(k,w,x,y,z)->k<=a) clientes ++ [(a,b,c,d,e)] ++ filter (\(k,w,x,y,z)->k>a) clientes


geraListaClienMes :: Clientes -> MesAniversario -> [NomeCliente] 
geraListaClienMes clientes mes = sort (map (\(a,b,c,d,e)-> b) (filter (\(a,b,c,d,e)-> d==mes) (transformaLista clientes)))

meses :: Clientes -> ([MesAniversario],[MesAniversario]) 
meses clientes = (lista, (filter (\x -> (notElem x lista)) [Janeiro ..]))
  where lista = (map (\(a,b,c,d,e)-> d) (transformaLista clientes))

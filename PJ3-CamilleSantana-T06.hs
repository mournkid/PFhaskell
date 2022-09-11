-- Camille Sousa Meneses de Santana 
--202000024285
import Data.Char
--menu
type CodProd = Int
type NomeProd = String
type PrecoProd = Int -- centavos
type Produto = (CodProd,NomeProd,PrecoProd)
type Menu = [Produto]

--cliente 
type CodCliente = Int
type NomeCliente = String
type CategCliente = Char
type ConsumoAnual = Int -- centavos
type Cliente = (CodCliente,NomeCliente,CategCliente,ConsumoAnual) 
type Clientes = [Cliente] 
type Compra = Int

-- pedido 
type Quant = Int
type SolCliente = (CodProd,NomeProd,Quant)
type PedidoCliente = [SolCliente]
type Pedidos = [(CodCliente,PedidoCliente)]

cardapio :: Menu
cardapio = [(1, "Acaraje",500), (2,"Hamburguer",700), (3,"Misto",450), (4,"Suco",300), (5,"Agua de coco",300), (6,"Acai", 500), (7,"Sorvete",400), (8,"Mousse",450)]

fregueses :: Clientes
fregueses = [(8 , "Tereza" , 'A' , 40000) , (7 , "Francisco" , 'B' , 60000), (6, "Karol" ,  'A' , 20000), (5 , "Catarina" , 'C' , 170000) , (4 , "Cecilia" , 'D' , 250000) , (3 , "Tomas" , 'A' , 45000) , (2 , "Pedro" , 'C' , 200000) , (1 , "Fatima" , 'C' ,170000)]

pedidos :: Pedidos
pedidos = [(1, [(1, "Acaraje", 2) , (3, "Misto", 1)]) , (2 ,[(2,"Hamburguer", 1)]) , (3, [(7, "Sorvete", 3)]) , (4, [(4, "Suco", 2)]) , (5, [(6, "Acai", 5)]) , (6, [(5, "Agua de Coco", 1) , (8, "Mousse", 1)])]


adicionaProdMenu :: Menu -> Produto -> Menu
adicionaProdMenu menu produto  
  | existe menu produto == [] = produto : menu 
  | otherwise = error "esse produto ja existe"
    where existe :: Menu -> Produto -> Menu
          existe menu (codigo,nome,preco) = filter (\(a,b,c)-> a==codigo) menu 

removeProdMenu :: Menu -> CodProd -> Menu
removeProdMenu menu codigo 
  | filter (\(a,b,c)->a==codigo) menu == [] = error "esse codigo nao existe"
  | otherwise = filter (\(a,b,c)->a/=codigo) menu  

coletaProdMenu :: Menu -> CodProd -> Produto 
coletaProdMenu menu codigo = head (filter (\(a,b,c)->a==codigo) menu)

coletaCliente :: Clientes -> CodCliente -> Cliente
coletaCliente clientes codigo = head (filter (\(a,b,c,d)->a==codigo) clientes) 

atualizaConsumo :: Clientes -> CodCliente -> Compra -> Clientes 
atualizaConsumo clientes codigo compra = inicio ++ [termo] ++ final 
  where indexa = zip clientes [1..] 
        inicio = take ((pos indexa codigo)-1) clientes 
        termo = atualiza (clientes !! ((pos indexa codigo)-1)) compra
        final = drop (pos indexa codigo) clientes 
        atualiza (a,b,c,d) compra = (a,b,c,d+compra)
        pos :: [((Int,String,Char,Int),Int)] -> CodCliente -> Int
        pos xs codigo = (snd.head) (filter (\((a,b,c,d),x)->a==codigo) xs) 

atualizaClientes :: Clientes -> Clientes
atualizaClientes clientes = map atualiza clientes
         where atualiza (a,b,c,d)
                  |d < 50000 = (a,b,'A',0) 
                  |d >=50000 && d<150000 = (a,b,'B',0) 
                  |d >=150000 && d< 250000 = (a,b,'C',0) 
                  |d >=250000 && d<350000 = (a,b,'D',0) 
                  |otherwise = (a,b,'E',0)

adicionaProdPedido :: PedidoCliente -> Menu -> CodProd -> Quant -> PedidoCliente
adicionaProdPedido pedido menu codigo quant 
  | filter (\(a,b,c)->a==codigo) pedido == [] = (codigo,(produto menu codigo),quant) : pedido 
  | otherwise = inicio ++ [termo] ++ final  
    where produto :: Menu -> CodProd -> NomeProd
          produto menu codigo = (nome.head) (filter (\(a,b,c)->a==codigo) menu) 
               where nome (a,b,c) = b  
          atualiza :: (Int,String,Int) -> Quant -> (Int,String,Int)
          atualiza (a,b,c) quant = (a,b,c+quant)
          indexa = zip pedido [1..] 
          inicio = take ((pos indexa codigo)-1) pedido 
          termo = atualiza (pedido !! ((pos indexa codigo)-1)) quant
          final = drop (pos indexa codigo) pedido 
          pos :: [((Int,String,Int),Int)] -> CodProd -> Int
          pos xs cod = (snd.head) (filter (\((a,b,c),x)->a==cod) xs)

cancelaProduto :: PedidoCliente -> CodProd -> Quant -> PedidoCliente
cancelaProduto pedido codigo quant 
  | filter (\(a,b,c)-> c<=quant) (filter cod pedido) /= [] = filter remove pedido
  | otherwise = diminui (filter cod pedido) quant ++ filter remove pedido 
    where cod (a,b,c) = a == codigo  
          remove (a,b,c) = a /= codigo 
          diminui :: PedidoCliente -> Quant -> PedidoCliente
          diminui pedido quant = map (\(a,b,c)->(a,b,c-quant)) pedido

type Preco = Int
type ProdTotalizado = (CodProd, NomeProd, Quant, Preco)
type PedidoTotalizado = [ProdTotalizado]
geraPedidoImpressao :: PedidoCliente -> Menu -> PedidoTotalizado
geraPedidoImpressao pedido menu = map (\(a,b,c,d)->(a,b,c,d*c)) (zipa pedido menu)
  where zipa :: PedidoCliente -> Menu -> PedidoTotalizado 
        zipa pedido menu = map aux (zip pedido (listaPreco menu))
          where listaPreco :: Menu -> [Quant]
                listaPreco menu = map (\(a,b,c)-> c) menu  
                aux ((a,b,c),d) = (a,b,c,d)

type Desconto = Int
type PrecoFinal = Int
type Totalizacao = (Preco,Desconto,PrecoFinal)
totalPedido :: Clientes -> CodCliente -> PedidoTotalizado -> Totalizacao
totalPedido clientes codigo pedido = (preco pedido, (((preco pedido)*(desconto clientes codigo)) `div` 100), ((preco pedido) - (((preco pedido)*(desconto clientes codigo)) `div` 100)))  
  where preco :: PedidoTotalizado -> PrecoFinal
        preco pedido = foldr (+) 0 (listaPreco pedido)
        listaPreco :: PedidoTotalizado -> [Preco]
        listaPreco pedido = map (\(a,b,c,d)->d) pedido 
        desconto :: Clientes -> CodCliente -> Desconto
        desconto clientes codigo 
          |head (map aux (filter cod clientes)) == 'A' = 0
          |head (map aux (filter cod clientes)) == 'B' = 3
          |head (map aux (filter cod clientes)) == 'C' = 5
          |head (map aux (filter cod clientes)) == 'D' = 10
          |otherwise = 15
           where cod (a,b,c,d) = a == codigo 
                 aux (a,b,c,d) = c

entregaPedido :: Pedidos -> CodCliente -> Pedidos
entregaPedido pedidos codigo = filter (\(a,b)->a/=codigo) pedidos 

formataValor :: Int -> String 
formataValor x 
 |x < 10 = "....0.0" ++ show x
 |x >=10 && x < 100 = "....0." ++ show x
 |x >=100 && x < 1000 = "...." ++ show (x `div` 100) ++ "." ++ drop ((length (show x))-2) (show x)
 |x >=1000 && x < 10000 = "..." ++ show (x `div`100) ++ "." ++ drop ((length (show x))-2) (show x)
 |x >=10000 && x < 100000 = ".." ++ show (x `div`100) ++ "." ++ drop ((length (show x))-2) (show x)
 |otherwise = "." ++ show (x`div`100) ++ "." ++ drop ((length (show x))-2) (show x)

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

formataLinhas :: PedidoTotalizado -> String
formataLinhas pedido = concat (map formataLinha pedido)  

ordenaMenu :: Menu -> Menu 
ordenaMenu menu = foldr insOrd [] menu  
  where insOrd :: Produto -> Menu -> Menu 
        insOrd (cod,nome,preco) menu = takeWhile menor menu ++ [(cod,nome,preco)] ++ dropWhile menor menu 
          where menor (a,b,c) = a <= cod 

ordenaClientes :: Clientes -> Clientes
ordenaClientes clientes = foldr quickSort [] clientes
  where quickSort :: Cliente -> Clientes -> Clientes
        quickSort (a,b,c,d) clientes = filter (\(w,x,y,z)->x<=b) clientes ++ [(a,b,c,d)] ++ filter (\(w,x,y,z)->x>b) clientes


listaCardapioOrd :: Menu -> IO() 
listaCardapioOrd menu = putStr ("CARDAPIO\n\n" ++ (formataLinhasMenu.ordenaMenu) menu)

formataLinhaMenu :: Produto -> String 
formataLinhaMenu produto = (codigo.devolveCod) produto ++ "  " ++ (nome.devolveNome) produto ++ (formataValor.devolvePreco) produto ++ "\n"
       where codigo :: CodProd -> String
             codigo x 
               |x < 10 = "   " ++ show x 
               |x >= 10 && x < 100 = "  " ++ show x 
               |x >= 100 && x < 1000 = " " ++ show x 
               |x >=1000 && x < 10000 = show x 
               |otherwise = error "contem mais que 4 digitos"
             devolveCod :: Produto -> Int
             devolveCod (codProd,nomeProd,precoProd) = codProd
             devolveNome :: Produto -> String
             devolveNome (codProd,nomeProd,precoProd) = nomeProd
             devolvePreco :: Produto -> Int
             devolvePreco (codProd,nomeProd,precoProd) = precoProd
             nome :: NomeProd -> String
             nome nomeProd = nomeProd ++ replicate (25-(length nomeProd)) '.'

formataLinhasMenu :: Menu -> String
formataLinhasMenu menu = concat (map formataLinhaMenu menu)  

listaClientesOrd :: Clientes -> IO()
listaClientesOrd clientes = putStr ("CLIENTES\n\n" ++ "CATEGORIA A\n\n" ++ listaClientesCat1 (ordenaClientes clientes) 'A'  ++ "\nCATEGORIA B\n\n" ++ listaClientesCat1 (ordenaClientes clientes) 'B' ++ "\nCATEGORIA C\n\n" ++ listaClientesCat1 (ordenaClientes clientes) 'C' ++ "\nCATEGORIA D\n\n" ++ listaClientesCat1 (ordenaClientes clientes) 'D' ++ "\nCATEGORIA E\n\n" ++ listaClientesCat1 (ordenaClientes clientes) 'E' ++ "\n")

type Categoria = Char

listaClientesCat1 :: Clientes -> Categoria -> String
listaClientesCat1 clientes categoria 
  |formataLinhasCat clientes categoria == [] =  "Nao ha clientes para a categoria " ++ [categoria] ++ "\n"
  |otherwise =  formataLinhasCat clientes categoria

listaClientesCatOrd :: Clientes -> Categoria -> IO()
listaClientesCatOrd clientes categoria 
  |formataLinhasCat clientes categoria == [] = putStr ("Nao ha clientes para a categoria " ++ [categoria] ++ "\n")
  |otherwise = putStr ("CLIENTES\n\n" ++ "CATEGORIA " ++ [categoria] ++ "\n\n" ++ formataLinhasCat (ordenaClientes clientes) categoria)

formataLinhaCat :: Cliente -> String 
formataLinhaCat cliente = (codigo.devolveCod) cliente ++ "     " ++ (nome.devolveNome) cliente ++ "\n"
        where devolveCod :: Cliente -> Int
              devolveCod (codCliente,nomeCliente,categCliente,consumo) = codCliente
              devolveNome :: Cliente -> String
              devolveNome (codCliente,nomeCliente,categCliente,consumo) = nomeCliente
              nome :: NomeCliente -> String
              nome nomeCliente = nomeCliente ++ replicate (50-(length nomeCliente)) ' '
              codigo :: CodCliente -> String
              codigo x 
               |x >= 10 && x < 100 = "    " ++ show x 
               |x < 10 = "     " ++ show x 
               |x >= 100 && x < 1000 = "   " ++ show x 
               |x >= 1000 && x < 10000 = "  " ++ show x 
               |x >= 10000 && x < 100000 = " " ++ show x
               |x >= 100000 && x < 1000000 = show x 
               |otherwise = error "contem mais que 6 digitos"

formataLinhasCat :: Clientes -> Categoria -> String
formataLinhasCat clientes categ = concat (map formataLinhaCat (filter (\(a,b,c,d)->c==categ) clientes))
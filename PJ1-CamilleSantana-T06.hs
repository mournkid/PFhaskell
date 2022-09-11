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


--sistema do restaurante
adicionaProdMenu :: Menu -> Produto -> Menu
adicionaProdMenu cardapio produt
  |[x | (x,y,z)<-cardapio, (a,b,c)<-[produt], x==a] ==[] = produt : cardapio
  |otherwise = error "ja existe"

removeProdMenu :: Menu -> CodProd -> Menu 
removeProdMenu menu codigo  
  |[(x,y,z) | (x,y,z)<-menu, x==codigo] == [] = error "nao existe"
  |otherwise = [(x,y,z) | (x,y,z)<-menu, x/=codigo]

coletaProdMenu :: Menu -> CodProd -> [Produto]
coletaProdMenu menu cod = [(x,y,z) | (x,y,z)<-menu, x==cod]  

-- sistema de clientes 
adicionaCliente :: Clientes -> NomeCliente -> Clientes
adicionaCliente [] nome = [(1,nome,'A',0)]
adicionaCliente clientes nome =  (1+it,nome,'A',0) : clientes 
                     where it = codigo (posicao clientes 1) 
                           indexa :: [(Int,String,Char,Int)] -> [((Int,String,Char,Int),Int)]
                           indexa xs = zip xs [1..length xs]
                           posicao :: [(Int,String,Char,Int)] -> Int -> [(Int,String,Char,Int)] 
                           posicao ys y = [ a |(a,x)<-indexa ys, x==y]
                           codigo :: [(Int,String,Char,Int)]  -> Int
                           codigo ls  = head [a | (a,b,c,d)<-ls]

coletaCliente :: Clientes -> CodCliente -> [Cliente]
coletaCliente clientes cod = [(a,b,c,d) | (a,b,c,d)<-clientes, a==cod]  

atualizaConsumo :: Clientes -> CodCliente -> Compra -> Clientes
atualizaConsumo clientes cod compra = inicio ++ [termo] ++ final
                             where inicio = take ((pos clientes cod)-1) clientes
                                   termo = atualiza (clientes !! ((pos clientes cod)-1)) compra
                                   final = drop (pos clientes cod) clientes
                                   indexa :: [(Int,String,Char,Int)] -> [((Int,String,Char,Int),Int)]
                                   indexa xs = zip xs [1..length xs]
                                   pos :: [(Int,String,Char,Int)] -> Int -> Int
                                   pos rs g = head [ x |((a,b,c,d),x)<-indexa rs, a==g]
                                   atualiza :: (Int,String,Char,Int) -> Int -> (Int,String,Char,Int)
                                   atualiza (a,b,c,d) x = (a,b,c,d+x)
                                   

atualizaClientes :: Clientes -> Clientes
atualizaClientes clientes = [(a,b,atualizaCateg d,0) | (a,b,c,d)<-clientes]
    where atualizaCateg ::  Int -> Char
          atualizaCateg x
            |x < 50000 = 'A'
            |x >=50000 && x<150000 = 'B'
            |x >=150000 && x< 250000 = 'C'
            |x >=250000 && x<350000 = 'D'
            |otherwise = 'E'

--restaurante

adicionaProdPedido :: PedidoCliente -> Menu -> CodProd -> Quant -> PedidoCliente
adicionaProdPedido pedido menu cod quant
  |elem cod (codigosCardapio pedido) = [(a,b, quant +c) | (a,b,c)<-pedido, a==cod] ++ [(cod,b,c) | (a,b,c)<-pedido, a/=cod]
  |otherwise = (cod,(nome menu cod),quant) : pedido
        where nome :: Menu -> CodProd -> NomeProd
              nome menu cod = head [nome | (codigo,nome, preco)<-menu, cod==codigo] 
              codigosCardapio :: Menu -> [CodProd]
              codigosCardapio menu = [codProd | (codProd,nome,preco)<-menu]
        
cancelaProduto :: PedidoCliente -> CodProd -> Quant -> PedidoCliente
cancelaProduto pedido codigo quant 
  |head [c | (a,b,c)<-pedido,a==codigo] <= quant = [(a,b,c) | (a,b,c)<-pedido, a/=codigo]
  |otherwise = [(a,b,c-quant) | (a,b,c)<-pedido, a==codigo, quant<c] ++ remove pedido codigo
     where remove :: PedidoCliente -> CodProd -> PedidoCliente
           remove pedido codigo = [(a,b,c) | (a,b,c)<-pedido, a/=codigo] 

adicionaPedido :: Pedidos -> PedidoCliente -> CodCliente -> Pedidos
adicionaPedido pedidos pedidoCliente codigo = pedidos ++ [(codigo,pedidoCliente)]

type Preco = Int
type ProdTotalizado = (CodProd, NomeProd, Quant, Preco)
type PedidoTotalizado = [ProdTotalizado]

geraPedidoImpressao :: PedidoCliente -> Menu -> PedidoTotalizado
geraPedidoImpressao pedido menu = [(codProd,nomeProd,quant, (precoProd menu codProd)*quant) | (codProd,nomeProd,quant, preco)<-zipa pedido menu]                    
                      where precoProd :: Menu -> CodProd ->PrecoProd
                            precoProd menu cod = head [c | (a,b,c)<-menu, a==cod]
                            zipa :: PedidoCliente -> Menu -> PedidoTotalizado 
                            zipa pedido menu = [(codProd,nomeProd,quant, preco) |((codProd,nomeProd,quant),preco)<-zip pedido [c | (a,b,c)<-menu]]

type Desconto = Int
type PrecoFinal = Int
type Totalizacao = (Preco,Desconto,PrecoFinal)

totalPedido :: Clientes -> CodCliente -> PedidoTotalizado -> Totalizacao
totalPedido clientes codigo pedido = ((preco pedido),(desconto clientes codigo), ((preco pedido)-((preco pedido )*(desconto clientes codigo)) `div` 100)) 
               where desconto :: Clientes -> CodCliente -> Desconto
                     desconto clientes cod 
                       |head [c | (a,b,c,d)<-clientes,cod==a] == 'A' = 0
                       |head [c | (a,b,c,d)<-clientes,cod==a] == 'B' = 3
                       |head [c | (a,b,c,d)<-clientes,cod==a] == 'C' = 5
                       |head [c | (a,b,c,d)<-clientes,cod==a] == 'D' = 10
                       |otherwise = 15
                     preco :: PedidoTotalizado -> Preco
                     preco pedido = sum [ d | (a,b,c,d)<-pedido]

entregaPedido :: Pedidos -> CodCliente -> Pedidos 
entregaPedido pedidos codigo = [(a,b) | (a,b)<-pedidos,a/=codigo] 

formataValor :: Int -> String 
formataValor x 
 |x < 10 = "....0.0" ++ show x
 |x >=10 && x < 100 = "....0." ++ show x
 |x >=100 && x < 1000 = "...." ++ show (x `div`100) ++ ".00"
 |x >=1000 && x < 10000 = "..." ++ show (x `div`100) ++ ".00"
 |x >=10000 && x < 100000 = ".." ++ show (x `div`100) ++ ".00"
 |otherwise = "." ++ show (x`div`100) ++ ".00"


formataLinha :: ProdTotalizado -> String
formataLinha prod =  codigo (devolveCod prod) ++ "  " ++ show (devolveQuant prod) ++ "  " ++ nome (devolveNome prod) ++ formataValor (devolvePreco prod) ++ "\n"
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
formataLinhas pedido = concat [formataLinha (a,b,c,d)  | (a,b,c,d)<-pedido] 

formataTotal:: Totalizacao-> String
formataTotal totalizacao = "\n         Total...................." ++ formataValor (devolvePreco totalizacao) ++"\n" ++ "         Desconto................." ++ formataValor (devolveDesconto totalizacao) ++ "\n" ++ "         A pagar.................." ++ formataValor (devolvePrecoFinal totalizacao) ++ "\n"
             where devolvePreco :: Totalizacao -> Int
                   devolvePreco (preco,desconto,precoFinal) = preco
                   devolveDesconto :: Totalizacao -> Int
                   devolveDesconto (preco,desconto,precoFinal) = desconto
                   devolvePrecoFinal :: Totalizacao -> Int
                   devolvePrecoFinal (preco,desconto,precoFinal) = precoFinal

geraConta :: CodCliente -> Clientes -> Menu -> PedidoCliente -> IO ()
geraConta codigo clientes menu pedido = putStr ("PEDIDO " ++ show codigo ++ "\n\n" ++ "COD   QTD  PRODUTO                  PRECO\n\n" ++ formataLinhas (geraPedidoImpressao pedido menu) ++ "\n\n" ++ formataTotal (totalPedido clientes codigo(geraPedidoImpressao pedido menu)) )

listaCardapio :: Menu -> IO ()
listaCardapio menu = putStr ("CARDAPIO\n\n" ++ formataLinhasMenu menu)

formataLinhaMenu :: Produto -> String 
formataLinhaMenu produto = codigo (devolveCod produto) ++ "  " ++ nome (devolveNome produto) ++ formataValor (devolvePreco produto) ++ "\n"
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
formataLinhasMenu menu = concat [formataLinhaMenu (a,b,c)  | (a,b,c)<-menu]
 
type Categoria = Char

listaClientesCat :: Clientes -> Categoria -> IO ()
listaClientesCat clientes categoria 
  |formataLinhasCat clientes categoria == [] = putStr ("Nao ha clientes para a categoria " ++ [categoria] ++ "\n")
  |otherwise = putStr ("CATEGORIA " ++ [categoria] ++ "\n\n" ++ formataLinhasCat clientes categoria)

formataLinhaCat :: Cliente -> String 
formataLinhaCat cliente = codigo (devolveCod cliente) ++ "     " ++ nome (devolveNome cliente) ++ "\n"
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
formataLinhasCat clientes categ = concat [formataLinhaCat (a,b,c,d)  | (a,b,c,d)<-clientes, categ==c]

listaClientesCat1 :: Clientes -> Categoria -> String
listaClientesCat1 clientes categoria 
  |formataLinhasCat clientes categoria == [] =  "Nao ha clientes para a categoria " ++ [categoria] ++ "\n"
  |otherwise =  formataLinhasCat clientes categoria


listaClientes :: Clientes -> IO ()
listaClientes clientes = putStr ("CATEGORIA A\n\n" ++ listaClientesCat1 clientes 'A'  ++ "\nCATEGORIA B\n\n" ++ listaClientesCat1 clientes 'B' ++ "\nCATEGORIA C\n\n" ++ listaClientesCat1 clientes 'C' ++ "\nCATEGORIA D\n\n" ++ listaClientesCat1 clientes 'D' ++ "\nCATEGORIA E\n\n" ++ listaClientesCat1 clientes 'E' ++ "\n")











                                      
                                    
                                  



          




                           




                                






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
adicionaProdMenu [] x = [x]
adicionaProdMenu ((a,b,c):xs) (x,y,z) 
  | codIgual ((a,b,c):xs) (x,y,z) = error "ja existe um produto com este codigo"
  | otherwise = (x,y,z) : ((a,b,c):xs)
    where codIgual :: Menu -> Produto -> Bool
          codIgual [] _ = False 
          codIgual ((a,b,c):xs) (x,y,z) = a == x || codIgual xs (x,y,z)

removeProdMenu :: Menu -> CodProd -> Menu 
removeProdMenu [] _ = error "este codigo nao existe"
removeProdMenu ((a,b,c):xs) cod
  | codIgual ((a,b,c):xs) cod = remove ((a,b,c):xs) cod 
  | otherwise = error "este codigo nao existe"
    where codIgual :: Menu -> CodCliente -> Bool
          codIgual [] _ = False 
          codIgual ((a,b,c):xs) cod = a == cod || codIgual xs cod 
          remove :: Menu -> CodCliente -> Menu
          remove [] _ = []
          remove ((a,b,c):xs) cod
            |cod == a = xs 
            |otherwise = (a,b,c) : remove xs cod 

coletaProdMenu :: Menu -> CodProd -> Produto
coletaProdMenu ((a,b,c):xs) cod
  | a == cod = (a,b,c)
  |otherwise = coletaProdMenu xs cod 

coletaCliente :: Clientes -> CodCliente -> Cliente
coletaCliente ((a,b,c,d):xs) cod 
  |a == cod = (a,b,c,d)
  |otherwise = coletaCliente xs cod 


atualizaConsumo :: Clientes -> CodCliente -> Compra -> Clientes
atualizaConsumo ((a,b,c,d):xs) cod compra = inicio ++ [termo] ++ final 
    where indexa = zip ((a,b,c,d):xs) [1..]
          pos :: [((Int,String,Char,Int),Int)] -> CodCliente -> Int
          pos [] _ = 0
          pos (((a,b,c,d),n):xs) cod 
            | a == cod = n 
            | otherwise = pos xs cod 
          inicio = take ((pos indexa cod)-1) ((a,b,c,d):xs)
          termo = atualiza (((a,b,c,d):xs) !! ((pos indexa cod)-1)) compra
          final = drop (pos indexa cod) ((a,b,c,d):xs)
          atualiza :: Cliente -> Compra -> Cliente 
          atualiza (a,b,c,d) compra = (a,b,c,d+compra)

atualizaClientes :: Clientes -> Clientes
atualizaClientes [] = []
atualizaClientes ((a,b,c,d):xs) = atualiza (a,b,c,d) : atualizaClientes xs 
          where atualiza :: Cliente -> Cliente
                atualiza (a,b,c,d)
                  |d < 50000 = (a,b,'A',0) 
                  |d >=50000 && d<150000 = (a,b,'B',0) 
                  |d >=150000 && d< 250000 = (a,b,'C',0) 
                  |d >=250000 && d<350000 = (a,b,'D',0) 
                  |otherwise = (a,b,'E',0) 

adicionaProdPedido :: PedidoCliente -> Menu -> CodProd -> Quant -> PedidoCliente 
adicionaProdPedido [] ((x,y,z):ys) codigo quant = pedido ((x,y,z):ys) codigo quant
    where pedido :: Menu -> CodProd -> Quant -> [(Int,String,Int)]
          pedido ((x,y,z):ys) codigo quant 
             | x == codigo = [(x,y,quant)]
             |otherwise = pedido ys codigo quant
adicionaProdPedido ((a,b,c):xs) ((x,y,z):ys) codigo quant
  |codIgual codigo ((a,b,c):xs) = inicio ++ [termo] ++ final
  |otherwise = pedido ((x,y,z):ys) codigo quant ++ ((a,b,c):xs)
     where indexa = zip ((a,b,c):xs) [1..]
           pos :: [((Int,String,Int),Int)] -> CodProd -> Int
           pos [] _ = 0 
           pos (((a,b,c),n):xs) cod 
            | a == cod = n 
            | otherwise = pos xs cod
           inicio = take ((pos indexa codigo)-1) ((a,b,c):xs)
           termo = atualiza (((a,b,c):xs) !! ((pos indexa codigo)-1)) quant
           final = drop (pos indexa codigo) ((a,b,c):xs)
           atualiza :: (Int,String,Int) -> Quant -> (Int,String,Int)
           atualiza (a,b,c) quant = (a,b,c+quant)
           pedido ((x,y,z):ys) codigo quant 
             | x == codigo = [(x,y,quant)]
             |otherwise = pedido ys codigo quant
           codIgual :: CodProd -> PedidoCliente -> Bool
           codIgual _ [] = False 
           codIgual codigo ((a,b,c):xs) = a == codigo || codIgual codigo xs  

cancelaProduto :: PedidoCliente -> CodProd -> Quant -> PedidoCliente
cancelaProduto ((a,b,c):xs) cod quant 
  | quantidade ((a,b,c):xs) cod <= quant = remove ((a,b,c):xs) cod
  | otherwise = diminui ((a,b,c):xs) cod quant
    where quantidade :: PedidoCliente -> CodProd -> Quant
          quantidade [] _ = 0 
          quantidade ((a,b,c):xs) cod 
            | a == cod = c 
            |otherwise = quantidade xs cod 
          remove :: PedidoCliente -> CodProd -> PedidoCliente
          remove [] _ = []
          remove ((a,b,c):xs) cod
            | a == cod = xs 
            | otherwise = (a,b,c) : remove xs cod 
          diminui :: PedidoCliente -> CodProd -> Quant -> PedidoCliente
          diminui [] _ _ = []
          diminui ((a,b,c):xs) cod quant
            | a == cod = ((a,b,c-quant):xs)
            | otherwise = (a,b,c) : diminui xs cod quant

type Preco = Int
type ProdTotalizado = (CodProd, NomeProd, Quant, Preco)
type PedidoTotalizado = [ProdTotalizado]
geraPedidoImpressao :: PedidoCliente -> Menu -> PedidoTotalizado
geraPedidoImpressao [] _ = []
geraPedidoImpressao _ [] = []
geraPedidoImpressao ((a,b,c):xs) ((x,y,z):ys) = zipa ((a,b,c):xs) ((x,y,z):ys)
           where zipa :: PedidoCliente -> Menu -> [(Int,String,Int,Int)]
                 zipa [] _ = []
                 zipa _ [] = []
                 zipa ((a,b,c):xs) ((x,y,z):ys) = (a,b,c,(preco ((x,y,z):ys) a)*c) : zipa xs ys
                 preco :: Menu -> CodProd -> PrecoProd
                 preco ((x,y,z):ys) cod 
                   |x == cod = z 
                   |otherwise = preco ys cod 

type Desconto = Int
type PrecoFinal = Int
type Totalizacao = (Preco,Desconto,PrecoFinal)
totalPedido :: Clientes -> CodCliente -> PedidoTotalizado -> Totalizacao
totalPedido ((a,b,c,d):xs) codigo ((x,w,y,z):ys) = (preco ((x,w,y,z):ys),desconto ((a,b,c,d):xs) codigo, ((preco ((x,w,y,z):ys)) - (((preco ((x,w,y,z):ys))*(desconto ((a,b,c,d):xs) codigo)) `div` 100)))
         where preco :: PedidoTotalizado -> Int
               preco [] = 0
               preco ((x,w,y,z):ys) = z + preco ys
               desconto :: Clientes -> CodCliente -> Int
               desconto [] _ = error "nao existe esse cliente"
               desconto ((a,b,c,d):xs) codigo 
                 |a == codigo && c == 'A' = 0
                 |a == codigo && c == 'B' = 3
                 |a == codigo && c == 'C' = 5 
                 |a == codigo && c == 'D' = 10
                 |a == codigo && c == 'E' = 15
                 |otherwise = desconto xs codigo


entregaPedido :: Pedidos-> CodCliente -> Pedidos
entregaPedido [] _ = []
entregaPedido ((a,b):xs) codigo
  |a == codigo = xs 
  |otherwise = (a,b): entregaPedido xs codigo 


formataValor :: Int -> String 
formataValor x 
 |x < 10 = "....0.0" ++ show x
 |x >=10 && x < 100 = "....0." ++ show x
 |x >=100 && x < 1000 = "...." ++ show (x `div` 100) ++ "." ++ drop ((length (show x))-2) (show x)
 |x >=1000 && x < 10000 = "..." ++ show (x `div`100) ++ "." ++ drop ((length (show x))-2) (show x)
 |x >=10000 && x < 100000 = ".." ++ show (x `div`100) ++ "." ++ drop ((length (show x))-2) (show x)
 |otherwise = "." ++ show (x`div`100) ++ "." ++ drop ((length (show x))-2) (show x)

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
formataLinhas [] = []
formataLinhas ((a,b,c,d):xs) = formataLinha (a,b,c,d) ++ formataLinhas xs 

incorporaCompra:: Clientes -> CodCliente -> PedidoTotalizado -> Clientes 
incorporaCompra ((a,b,c,d):xs) codigo ((w,x,y,z):ys) = atualizaConsumo ((a,b,c,d):xs) (codigo) (preco (totalPedido ((a,b,c,d):xs) codigo ((w,x,y,z):ys)))
        where preco :: Totalizacao -> PrecoFinal
              preco (a,b,c) = c 

ordenaMenu :: Menu -> Menu
ordenaMenu [] = []
ordenaMenu ((a,b,c):xs) = insOrd (a,b,c) (ordenaMenu xs)
    where insOrd :: Produto -> Menu -> Menu
          insOrd (x,y,z) [] = [(x,y,z)]
          insOrd (x,y,z) ((a,b,c):xs) 
           | x <= a = (x,y,z) : (a,b,c) : xs
           | otherwise = (a,b,c) : insOrd (x,y,z) xs 


ordenaClientes :: Clientes -> Clientes
ordenaClientes [] = []
ordenaClientes ((a,b,c,d):xs) = ordenaClientes (us xs b) ++ [(a,b,c,d)] ++ ordenaClientes (vs xs b)
     where us :: Clientes -> NomeCliente -> Clientes
           us [] _ = []
           us ((a,b,c,d):xs) nome 
             | b <= nome = (a,b,c,d) : us xs nome 
             | otherwise = us xs nome 
           vs :: Clientes -> NomeCliente -> Clientes
           vs [] _ = []
           vs ((a,b,c,d):xs) nome 
             | b > nome = (a,b,c,d) : vs xs nome 
             | otherwise = vs xs nome   

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
formataLinhasMenu [] = []
formataLinhasMenu ((a,b,c):xs) = formataLinhaMenu (a,b,c) ++ formataLinhasMenu xs  


listaCardapioOrd :: Menu -> IO()
listaCardapioOrd menu = putStr ("CARDAPIO\n\n" ++ formataLinhasMenu (ordenaMenu menu))

type Categoria = Char

listaClientesCatOrd :: Clientes -> Categoria -> IO ()
listaClientesCatOrd clientes categoria 
  |formataLinhasCat clientes categoria == [] = putStr ("Nao ha clientes para a categoria " ++ [categoria] ++ "\n")
  |otherwise = putStr ("CATEGORIA " ++ [categoria] ++ "\n\n" ++ formataLinhasCat (ordenaClientes clientes) categoria)

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
formataLinhasCat [] _ = []
formataLinhasCat ((a,b,c,d):xs) categ 
  | categ == c = formataLinhaCat (a,b,c,d) ++ formataLinhasCat xs categ
  | otherwise = formataLinhasCat xs categ

listaClientesOrd :: Clientes -> IO()
listaClientesOrd clientes = putStr ("CATEGORIA A\n\n" ++ listaClientesCat1 (ordenaClientes clientes) 'A'  ++ "\nCATEGORIA B\n\n" ++ listaClientesCat1 (ordenaClientes clientes) 'B' ++ "\nCATEGORIA C\n\n" ++ listaClientesCat1 (ordenaClientes clientes) 'C' ++ "\nCATEGORIA D\n\n" ++ listaClientesCat1 (ordenaClientes clientes) 'D' ++ "\nCATEGORIA E\n\n" ++ listaClientesCat1 (ordenaClientes clientes) 'E' ++ "\n")

listaClientesCat1 :: Clientes -> Categoria -> String
listaClientesCat1 clientes categoria 
  |formataLinhasCat clientes categoria == [] =  "Nao ha clientes para a categoria " ++ [categoria] ++ "\n"
  |otherwise =  formataLinhasCat clientes categoria
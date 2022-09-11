calculo :: [Int]
calculo = [x+y | x <- [1..4], y <- [2..4], x>y]

put3times :: String -> IO ()
put3times str = do
                   putStrLn str
                   putStrLn str
                   putStrLn str

putNtimes :: Int -> String -> IO ()
putNtimes n str = if (n<=1)
        then putStrLn str
        else do
                putStrLn str
                putNtimes (n-1) str

{-put3times = putNtimes 3-}


read2lines :: IO ()
read2lines = do
               getLine
               getLine
               putStr "Duas linhas lidas."

getInt :: IO Int
getInt = do line <- getLine
            return (read line :: Int)


add2Ints :: IO ()
add2Ints = do
             n1 <- getInt
             n2 <- getInt
             putStrLn (show (n1+n2))

add2Ints' :: IO ()
add2Ints' = do
             n1 <- getInt
             n2 <- getInt
             let summ = n1+n2
             putStrLn (show summ)



copy :: IO ()
copy = do str <- getLine
          putStrLn str
          copy


copyN :: Integer -> IO ()
copyN n = if n<=0 
           then return ()
           else do str <- getLine
                   putStrLn str
                   copyN (n-1)

copyEmpty :: IO ()
copyEmpty = do str <- getLine
               if str == ""
                then return ()
                else do putStrLn str
               copyEmpty

copyCount :: Integer -> IO ()
copyCount n = do str <- getLine
                 if str == ""
                  then putStrLn (show n ++ "linhas copiadas.")
                  else do putStrLn str
                 copyCount (n+1)

while :: Bool -> IO() -> IO()
while test action = do 
                       if test 
                       then do action
                               while test action
                       else return ()

copyInpOut :: IO ()
copyInpOut = while True (do  line <- getLine
	                         putStrLn line)

{-somaInts :: Int -> Int -> IO Int
somaInts n res = if n <= 0
                  then return res
                  else 
                  do m <- getInt
                 somaInts (n-1) (res+m)-}


{-main :: IO ()
main = do n <- getInt
          val <- somaInts n 0
          putStr $ show val ++ "\n"-}


{-getListReversed :: Int -> IO [Int]
getListReversed n =
                    if n==0
                     then return []
                     else do xs <- (getListReversed (n-1))
                             x <- getInt
                          return (x:xs)-}






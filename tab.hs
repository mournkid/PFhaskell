quantidade :: Int -> [Integer]
quantidade 0 = []
quantidade x = (quantidade (x-1)) ++ 2^x:[]

casas :: Integer
casas = sum (1:quantidade 63)
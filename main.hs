type Matrix a = [[a]]

-- Взима матрица, и премахва най-горния ред и n-тата колона
f ::(Num a) => Int -> Matrix a -> Matrix a 
f n m = (map . map) snd $ filter ((/= n) . fst) <$> zip [0..] <$> (tail m)

calcDeterminant :: (Fractional a, Num a) => Matrix a -> a
-- Базов случай, детерминантата на 1x1 матрица е числото вътре
calcDeterminant [[x]] = x 
-- Реукрсивно намаля реда и калкулира сумата от всички детерминанти
calcDeterminant m = sum [(-1)^n*v*calcDeterminant (f n m) | (n, v) <- zip [0..] (head m)]

main :: IO ()
main = interact $ show . calcDeterminant . map (map read . words) . lines

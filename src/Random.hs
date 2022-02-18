module Random where
import System.IO.Unsafe
import System.Random

randomNumber :: Int -> Int -> Int
randomNumber a b = unsafePerformIO (getStdRandom (randomR (a, b)))

resetrandomtwo :: Int -> Int -> Int -> Int
resetrandomtwo nbr max min
    | nbr > max = resetrandomtwo (max - nbr) max min
    | nbr < min = resetrandomtwo (min + nbr) max min
    | otherwise = nbr

resetrandom:: Int -> Int -> Int -> Int
resetrandom nbr max min = resetrandomtwo (nbr + 13) max min
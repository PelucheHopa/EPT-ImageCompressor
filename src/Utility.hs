module Utility where
import System.Exit ()
import System.Environment ( getArgs )
import System.IO ()
import Args

{-CONCAT EVERYTHING-}
myConcat :: [a] -> [a] -> [a]
myConcat xs ys = foldr (:) ys xs

takeFirst :: [a] -> a
takeFirst [x] = x
takeFirst (x:xs) = x

removeFirst :: [a] -> [a]
removeFirst [] = []
removeFirst [x] = [x]
removeFirst (x:xs) = xs

removeLast :: [a] -> [a]
removeLast [] = []
removeLast [x] = []
removeLast (x:xs) = myConcat [x] (removeLast xs)

removeLastif :: String -> String
removeLastif [] = []
removeLastif [x]
    | [x] == "\n" = []
    | otherwise = [x]
removeLastif (x:xs) = myConcat [x] (removeLast xs)

myList :: a -> b -> (a,b)
myList xs ys = (xs,ys)

takeline :: String -> String -> String

takeline  [ss] [] = ""
takeline  [] [xs] = ""
takeline  (s:ss) [] = s:ss
takeline  [] (x:xs)
    | x /= '\n' = takeline (myConcat [] [x]) xs
takeline [] [] = ""
takeline (s:ss) [x] = s:ss
takeline [s] (x:xs)
    | x /= '\n' = takeline (myConcat [s] [x]) xs
takeline (s:ss) (x:xs)
    | x /= '\n' = takeline (myConcat (s:ss) [x]) xs
    | otherwise = s:ss

removeline :: String -> String
removeline [] = ""
removeline [x] = ""
removeline (x:xs)
    | x /= '\n' = removeline xs
    | otherwise = xs

gotolist :: a -> [a]
gotolist a = [a]

intFloat :: Int -> Float
intFloat nbr = fromIntegral nbr :: Float

strInt :: String -> Int 
strInt str = read str :: Int 
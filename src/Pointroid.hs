module Pointroid where
import System.Exit
import System.Environment
import Utility
import CPG
import Random
import Launch

removeUseless :: String -> Char -> String
removeUseless [] a = ""
removeUseless [xs] a = ""
removeUseless (x:xs) a
    | x /= a = removeUseless xs a
    | otherwise = xs

takeX :: String -> String -> String
takeX [] result = []
takeX [xs] result = result
takeX (x:xs) result
    | x == '(' = takeX xs result
    | x /= ',' = takeX xs (myConcat result [x])
    | otherwise = result

takeY :: String -> String -> String
takeY [] result = []
takeY [xs] result = result
takeY (x:xs) result
    | x == '(' = takeY xs result
    | x /= ',' = takeY xs (myConcat result [x])
    | otherwise = result

takeZ :: String -> String -> String
takeZ [] result = []
takeZ [xs] result = result
takeZ (x:xs) result
    | x == ')' = takeY xs result
    | x /= ',' = takeY xs (myConcat result [x])
    | otherwise = result

stringToInt :: String -> Int
stringToInt str = read str :: Int

createPoint :: String -> Pointroide
createPoint str = Point { 
str=str,
id_centroide=0, value=0,
p_x=takeX (removeUseless str ' ') "",
p_y=takeY (removeUseless (removeUseless str ' ') ',') "",
p_z=takeZ (removeUseless (removeUseless (removeUseless str ' ') ',') ',') ""}

createListPoint :: String -> [Pointroide] -> [Pointroide]
createListPoint [] a =  a
createListPoint str a =  createListPoint (removeline str) (myConcat a (gotolist (createPoint (takeline "" str))))

displayPoint :: [Pointroide] -> String
displayPoint [] = ""
displayPoint [xs] = str xs
displayPoint (x:xs) = myConcat (displayPoint xs) (myConcat "\n" (str x))

displayValue :: [Pointroide] -> [Float] -> [Float]
displayValue [] _ = []
displayValue [xs] _ = gotolist (value xs)
displayValue (x:xs) list = myConcat (displayValue xs list) (myConcat list (gotolist (value x)))

displayID :: [Pointroide] -> [Int] -> [Int]
displayID [] _ = []
displayID [xs] _ = gotolist (id_centroide xs)
displayID (x:xs) list = myConcat (displayID xs list) (myConcat list (gotolist (id_centroide x)))

createCentroide :: Int -> Int -> Centroide
createCentroide nbr id = Center { c_id=id, c_x=resetrandom nbr 255 1, c_y=resetrandom (resetrandom nbr 255 1) 255 1, c_z=resetrandom (resetrandom (resetrandom nbr 255 1) 255 1) 255 1}

createListCentroide :: Int -> [Centroide] -> Int -> [Centroide]
createListCentroide nbr a rd
    | nbr > 0 = createListCentroide (nbr - 1) (myConcat a (gotolist (createCentroide (resetrandom rd  255 1) nbr))) (resetrandom rd 255 1)
    | otherwise = a

displayCentroideX :: [Centroide] -> [Int] -> [Int]
displayCentroideX [] _ = []
displayCentroideX [xs] list = myConcat list (gotolist (c_x xs))
displayCentroideX (x:xs) list = displayCentroideX xs (myConcat list (gotolist (c_x x)))

displayCentroideY :: [Centroide] -> [Int] -> [Int]
displayCentroideY [] _ = []
displayCentroideY [xs] list = myConcat list (gotolist (c_y xs))
displayCentroideY (x:xs) list = displayCentroideY xs (myConcat list (gotolist (c_y x)))

displayCentroideZ :: [Centroide] -> [Int] -> [Int]
displayCentroideZ [] _ = []
displayCentroideZ [xs] list = myConcat list (gotolist (c_z xs))
displayCentroideZ (x:xs) list = displayCentroideZ xs (myConcat list (gotolist (c_z x)))

displayCentroideID :: [Centroide] -> [Int] -> [Int]
displayCentroideID [] _ = []
displayCentroideID [xs] list = myConcat list (gotolist (c_id xs))
displayCentroideID (x:xs) list = displayCentroideID xs (myConcat list (gotolist (c_id x)))

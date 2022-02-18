module Launch where
import System.Exit
import System.Environment
import Utility
import CPG
import Random

{-FAIT LA MOYENNE-}
moyX :: [Pointroide] -> Int -> Int -> Int -> Int
moyX [] total nbr id
    | nbr > 1 = total `div` (nbr - 1)
    | otherwise = 0
moyX [x] total nbr id
    | id == id_centroide x = moyX [] (total + strInt (p_x x)) (nbr + 1) id
    | otherwise = moyX [] total nbr id
moyX (x:xs) total nbr id 
    | id == id_centroide x = moyX xs (total + strInt (p_x x)) (nbr + 1) id
    | otherwise = moyX xs total nbr id

moyY :: [Pointroide] -> Int -> Int -> Int -> Int
moyY [] total nbr id
    | nbr > 1 = total `div` (nbr - 1)
    | otherwise = 0
moyY [x] total nbr id
    | id == id_centroide x = moyY [] (total + strInt (p_y x)) (nbr + 1) id
    | otherwise = moyY [] total nbr id
moyY (x:xs) total nbr id 
    | id == id_centroide x = moyY xs (total + strInt (p_y x)) (nbr + 1) id
    | otherwise = moyY xs total nbr id

moyZ :: [Pointroide] -> Int -> Int -> Int -> Int
moyZ [] total nbr id
    | nbr > 1 = total `div` (nbr - 1)
    | otherwise = 0
moyZ [x] total nbr id
    | id == id_centroide x = moyZ [] (total + strInt (p_z x)) (nbr + 1) id
    | otherwise = moyZ [] total nbr id
moyZ (x:xs) total nbr id 
    | id == id_centroide x = moyZ xs (total + strInt (p_z x)) (nbr + 1) id
    | otherwise = moyZ xs total nbr id

{-fait la formule-}
rsquare :: Pointroide -> Centroide -> Float
rsquare p c = sqrt (((intFloat (strInt (p_x p)) - intFloat (c_x c))^2) +
                    ((intFloat (strInt (p_y p)) - intFloat (c_y c))^2) +
                    ((intFloat (strInt (p_z p)) - intFloat (c_z c))^2))

{-check si le centroide a changer-}
checkCentroide :: Centroide -> Centroide -> Bool 
checkCentroide a b
    | (c_x a == c_x b) && (c_y a == c_y b) && (c_z a == c_z b) = True 
    | otherwise = False 

{-compare tout les centroide et return le result-}
compareAllResult :: [Centroide] -> Pointroide -> Float -> Float
compareAllResult [] _ _ = 0
compareAllResult [x] p act
    | act > rsquare p x = rsquare p x
    | otherwise = act
compareAllResult (x:xs) p act
    | act > rsquare p x = compareAllResult xs p (rsquare p x)
    | otherwise = compareAllResult xs p act

{-compare tout les centroides pour l'id correspondant-}
compareAllCentroide :: [Centroide] -> Pointroide -> Float -> Int -> Int
compareAllCentroide [] _ _ _ = -1
compareAllCentroide [x] p min act
    | min > rsquare p x = c_id x
    | otherwise = act
compareAllCentroide (x:xs) p min act
    | min > rsquare p x = compareAllCentroide xs p (rsquare p x) (c_id x)
    | otherwise = compareAllCentroide xs p min act

{-Creer les nv point via ancienne list-}
createPointbyPoint :: Pointroide -> [Centroide] -> Pointroide
createPointbyPoint p lc = Point {str=str p, id_centroide=compareAllCentroide lc p 1000 0, value=compareAllResult lc p 1000, p_x=p_x p, p_y=p_y p, p_z=p_z p}

createListPointbyPoint :: [Pointroide] -> [Centroide] -> [Pointroide] -> [Pointroide]
createListPointbyPoint [] lc lp = lp
createListPointbyPoint [x] lc lp = createListPointbyPoint [] lc (myConcat lp (gotolist (createPointbyPoint x lc)))
createListPointbyPoint (x:xs) lc lp = createListPointbyPoint xs lc (myConcat lp (gotolist (createPointbyPoint x lc)))

{-Creer les nv pcentre via la list de pointroide pour les moyennes-}
createCentroidebyPoint :: [Pointroide] -> Int -> Centroide
createCentroidebyPoint pde id = Center { c_id=id, c_x=moyX pde 0 1 id, c_y=moyY pde 0 1 id, c_z=moyZ pde 0 1 id}

createNewListCentroide :: Int -> [Centroide] -> [Pointroide] -> [Centroide]
createNewListCentroide nbr list point
    | nbr > 0 = createNewListCentroide (nbr - 1) (myConcat list (gotolist (createCentroidebyPoint point nbr))) point
    | otherwise = list

launchPointroidetwo :: [Pointroide] -> [Centroide] -> [Centroide] -> Int -> [Pointroide]
launchPointroidetwo lp _ [] max = lp
launchPointroidetwo lp [] _ max = lp
launchPointroidetwo lp [c] _ max = lp
launchPointroidetwo lp (c:cs) (n:ns) max
    | not (checkCentroide c n) = launchPointroidetwo (createListPointbyPoint lp (c:cs) []) (createNewListCentroide max [] (createListPointbyPoint lp (c:cs) [])) (c:cs) max
    | otherwise = lp

launchPointroide :: [Pointroide] -> [Centroide] -> [Centroide] -> Int -> [Pointroide]
launchPointroide [] _ _ _ = []
launchPointroide _ [] _ _ = []
launchPointroide lp [c] [] max = launchPointroidetwo (createListPointbyPoint lp [c] []) (createNewListCentroide max [] (createListPointbyPoint lp [c] [])) [c] max
launchPointroide lp (c:cs) [] max = launchPointroidetwo (createListPointbyPoint lp (c:cs) []) (createNewListCentroide max [] (createListPointbyPoint lp (c:cs) [])) (c:cs) max
launchPointroide lp [c] (n:ns) max = launchPointroidetwo (createListPointbyPoint lp [c] []) (createNewListCentroide max [] (createListPointbyPoint lp [c] [])) [c] max
launchPointroide lp (c:cs) (n:ns) max = launchPointroidetwo (createListPointbyPoint lp (c:cs) []) (createNewListCentroide max [] (createListPointbyPoint lp (c:cs) [])) (c:cs) max

launchCentroidetwo :: [Pointroide] -> [Centroide] -> [Centroide] -> Int -> [Centroide]
launchCentroidetwo [] _ _ max = []
launchCentroidetwo lp [] _ max = []
launchCentroidetwo lp _ [] max = []
launchCentroidetwo lp [c] _ max = createNewListCentroide max [] (createListPointbyPoint lp [c] [])
launchCentroidetwo lp (c:cs) (n:ns) max
    | not (checkCentroide c n) = launchCentroidetwo (createListPointbyPoint lp (c:cs) []) (createNewListCentroide max [] (createListPointbyPoint lp (c:cs) [])) (c:cs) max
    | otherwise = createNewListCentroide max [] (createListPointbyPoint lp (c:cs) [])

launchCentroide :: [Pointroide] -> [Centroide] -> [Centroide] -> Int -> [Centroide]
launchCentroide [] _ _ _ = []
launchCentroide _ [] _ _ = []
launchCentroide lp [c] [] max = launchCentroidetwo (createListPointbyPoint lp [c] []) (createNewListCentroide max [] (createListPointbyPoint lp [c] [])) [c] max
launchCentroide lp (c:cs) [] max = launchCentroidetwo (createListPointbyPoint lp (c:cs) []) (createNewListCentroide max [] (createListPointbyPoint lp (c:cs) [])) (c:cs) max
launchCentroide lp [c] (n:ns) max = launchCentroidetwo (createListPointbyPoint lp [c] []) (createNewListCentroide max [] (createListPointbyPoint lp [c] [])) [c] max
launchCentroide lp (c:cs) (n:ns) max = launchCentroidetwo (createListPointbyPoint lp (c:cs) []) (createNewListCentroide max [] (createListPointbyPoint lp (c:cs) [])) (c:cs) max

formatNewPart :: Centroide -> String 
formatNewPart c = myConcat (myConcat (myConcat (myConcat (myConcat (myConcat  "--\n(" (show (c_x c) :: String)) ",") (show (c_y c) :: String)) ",") (show (c_z c) :: String)) ")\n-\n"

displayResult :: [Pointroide] -> [Centroide] -> String -> [Pointroide] -> String
displayResult _ [] list reset = list
displayResult [] _ list reset = list
displayResult [p] [c] list reset
    | id_centroide p == c_id c = displayResult [] [] (myConcat (myConcat list (str p)) "\n") reset
    | otherwise = displayResult [] [] list reset
displayResult (p:ps) [c] list reset
    | id_centroide p == c_id c = displayResult ps [c] (myConcat (myConcat list (str p)) "\n") reset
    | otherwise = displayResult ps [c] list reset
displayResult [p] (c:cs) list reset
    | id_centroide p == c_id c = displayResult reset cs (myConcat (myConcat list (str p)) "\n") reset
    | otherwise = displayResult reset cs (myConcat list (formatNewPart (takeFirst cs))) reset
displayResult (p:ps) (c:cs) list reset
    | id_centroide p == c_id c = displayResult ps (c:cs) (myConcat (myConcat list (str p)) "\n") reset
    | otherwise = displayResult ps (c:cs) list reset
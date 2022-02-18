module Args where
import System.Exit
import System.Environment ( getArgs )
import System.IO

separeArg :: [a] -> a
separeArg [x] = x
separeArg (x:xs) = x

takeNoption :: [String] -> Int
takeNoption [] = -1
takeNoption [xs] = -1
takeNoption (x:as:xs)
    | x == "-n" && (read as :: Int) == 0 = 0
    | x == "-n" && (read as :: Int) == 1 = 1
    | x == "-n" = (read as :: Int) + 1
    | otherwise = takeNoption (as:xs)

takeConvoption :: [String] -> String
takeConvoption [] = ""
takeConvoption [xs] = ""
takeConvoption (x:as:xs)
    | x == "-l" = as
    | otherwise = takeConvoption (as:xs)

takeFileoption :: [String] -> String
takeFileoption [] = ""
takeFileoption [xs] = ""
takeFileoption (x:xs)
    | x == "-f" = separeArg xs
    | otherwise = takeFileoption xs

module Main where
import System.Environment ( getArgs )
import System.IO
import System.Exit
import Pointroid
import Random
import CPG
import Utility
import Args
import System.IO.Unsafe
import System.Random
import Launch

leave :: Global -> IO ()
leave a
    | number_option a == 0 = exitWith (ExitFailure 84)
    | conv_option a == "" = exitWith (ExitFailure 84)
    | file_option a == "" = exitWith (ExitFailure 84)
    | otherwise = return()

main :: IO ()
main = do
    {- TAKE ARGS-}
    (x:xs) <- getArgs
    {-variable-}
    g <- getStdGen
    let point = Point { str="", id_centroide=0, value=0, p_x="", p_y="", p_z=""}
    let center = Center { c_id=0, c_x=0, c_y=0, c_z=0}
    let global = Global { number_option=takeNoption (x:xs), conv_option=takeConvoption (x:xs), file_option=takeFileoption (x:xs), seed=randomNumber 1 255}
    {-check args-}
    let check = leave global
    check
    {-openfile etc-}
    handle <- openFile (file_option global) ReadMode  
    contents <- hGetContents handle
    {- create list of pointroide-}
    let list_point = gotolist point
    let create_list_point = removeFirst (createListPoint (myConcat (removeLastif contents) "\0") list_point)
    {- create list of centroide -}
    let list_center = gotolist center
    let create_list_centroide = removeFirst (createListCentroide (number_option global) list_center (seed global))
    let compressPointroide = launchPointroide create_list_point create_list_centroide [] (number_option global)
    let compressCentroide = launchCentroide create_list_point create_list_centroide [] (number_option global)
    putStrLn (removeLast (displayResult compressPointroide compressCentroide (formatNewPart (takeFirst compressCentroide)) compressPointroide))
    hClose handle
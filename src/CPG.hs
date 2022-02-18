module CPG where
import System.Exit
import System.Environment ( getArgs )
import System.IO
import Utility

data Centroide = Center { 
    c_id :: Int,
    c_x :: Int,
    c_y :: Int,
    c_z :: Int
} deriving Show

data Pointroide = Point {
    str :: String,
    id_centroide :: Int ,
    value :: Float,
    p_x :: String,
    p_y :: String,
    p_z :: String
} deriving Show

data Global = Global {
{-    l_point :: [Pointroide],
    l_center :: [Centroide], -}
    number_option :: Int,
    conv_option :: String,
    file_option :: String,
    seed :: Int
} deriving Show

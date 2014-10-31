module MovielensData
(loadData
)
where

import Data.Text as T (pack, unpack, split, Text) 
import System.IO
import Data.Matrix
import Data.List.Split as S

main = do
     ts <- loadData filepath 
     putStrLn ("end of movielens main: " ++ (show (reduced_r (createR ts))))

filepath :: String
filepath = "/home/lukas/oschena/ml-100k/u1.base"
ypath = "/home/lukas/oschena/y.txt"

loadData::FilePath -> IO [[Int]]
loadData x = do
         contents <- readFile x
         return $ strs2int $ lines contents

loadMatrix :: FilePath -> IO (Matrix Int)
loadMatrix x = do
  contents <- readFile x
  return $ fromLists (strs2int $ lines contents)

strs2int :: [String] -> [[Int]]
strs2int xs = [str2ints x | x <- xs]

str2ints :: String -> [Int]
str2ints str = [read x | x <- tail (S.splitOn " " str)]

strs2texts :: [String] -> [[Int]]
strs2texts strlist = [init $ (texts2ints.str2text) x | x <- strlist ]

str2text :: String -> [Text]
str2text str = T.split (=='\t') (T.pack str)

texts2ints :: [Text] -> [Int]
texts2ints ts = [ read $ unpack x | x <- ts ]

nr_user :: Int
nr_user = 1682

nr_item :: Int
nr_item = 943

rnr_user = 4
rnr_item = 5

r :: Matrix Int
r = zero nr_user nr_item

createR :: [[Int]] -> Matrix Int
createR xs = foldl addRating r $ take 100 xs

addRating :: Matrix Int -> [Int] -> Matrix Int
addRating acc (u:i:r:xs) = setElem r (u, i) acc

reduced_r :: Matrix Int -> Matrix Int
reduced_r x = submatrix 1 rnr_item 1 rnr_user x

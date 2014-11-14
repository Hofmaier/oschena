module MovieLensData
(loadData,
loadMatrix,
ypath,
xpath,
thetapath,
xrpath,
yrpath,
thetarpath,
rpath,
rrpath
)
where

import Data.Text as T (pack, unpack, split, Text) 
import System.IO
import Data.Matrix
import Data.List.Split as S

main = do
     ts <- loadData filepath 
     putStrLn "end of movielens main: " 

filepath :: String
filepath = "/home/lukas/oschena/ml-100k/u1.base"

yrpath = "/home/lukas/mfbuild/y336x188.txt"
xrpath = "/home/lukas/mfbuild/x336x188.txt"
thetarpath = "/home/lukas/mfbuild/theta336x188.txt"
rrpath = "/home/lukas/mfbuild/r336x188.txt"

ypath = "/home/lukas/mfbuild/y1682x943.txt"
xpath = "/home/lukas/mfbuild/x1682x943.txt"
thetapath = "/home/lukas/mfbuild/theta1682x943.txt"
rpath = "/home/lukas/mfbuild/r1682x943.txt"

loadData::FilePath -> IO [[Float]]
loadData x = do
         contents <- readFile x
         return $ strs2int $ lines contents

loadMatrix :: FilePath -> IO (Matrix Float)
loadMatrix x = do
  contents <- readFile x
  return $ fromLists (strs2int $ lines contents)

strs2int :: [String] -> [[Float]]
strs2int xs = [str2ints x | x <- xs]

str2ints :: String -> [Float]
str2ints str = [read x | x <- strxs str]

strxs :: String -> [String]
strxs str = tail $ S.splitOn " " str

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

r = zero nr_user nr_item

--createR :: [[Int]] -> Matrix Float
--createR xs = foldl addRating r $ take 100 xs

addRating :: Matrix Int -> [Int] -> Matrix Int
addRating acc (u:i:r:xs) = setElem (fromIntegral r) (u, i) acc

reduced_r :: Matrix Float -> Matrix Float
reduced_r x = submatrix 1 rnr_item 1 rnr_user x

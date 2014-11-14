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
import qualified Data.MultiMap as Map
--import Math.Statistics (mean)

type UserItemRating = [Int]
type User = Int
type Rating = Int
type URdict = Map.MultiMap User Int

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
testsetpath = "../ml-100k/u1.test"

loadData::FilePath -> IO [[Float]]
loadData x = do
         contents <- readFile x
         return $ strs2fs $ lines contents

loadUirs :: FilePath -> IO [UserItemRating]
loadUirs fp = do 
  c <- readFile fp
  return $ strs2ints $ lines c

loadMatrix :: FilePath -> IO (Matrix Float)
loadMatrix x = do
  contents <- readFile x
  return $ fromLists (strs2fs $ lines contents)

strs2fs :: [String] -> [[Float]]
strs2fs xs = [str2fs x | x <- xs]

strs2ints :: [String] -> [UserItemRating]
strs2ints xs = [str2ints str| str <-  xs]

str2ints :: String -> [Int]
str2ints str = [read x | x <- strxs2 str]

strxs2 :: String -> [String]
strxs2 str = reverse $ tail $ reverse $ splitOn "\t" str

str2fs ::  String -> [Float]
str2fs str =  [read x | x <- strxs str ]

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

urdict :: Map.MultiMap User Rating
urdict = Map.empty

loadurdict :: [UserItemRating] -> URdict
loadurdict uirs = foldl insertr Map.empty uirs

insertr :: URdict -> UserItemRating -> URdict
insertr acc (u:i:r:_) = Map.insert u r acc

bu :: User -> URdict -> Float
bu u d = fromIntegral((sum (ratingsOfUser u d))) / fromIntegral ((length (ratingsOfUser u d)))

ratingsOfUser :: User -> URdict -> [Int]
ratingsOfUser u d = Map.lookup u d

uirsv :: [UserItemRating]
uirsv =  [[1,5,1],[1,5,2],[1,5,3]]

dv = loadurdict uirsv

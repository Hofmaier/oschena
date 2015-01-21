module MovieLensData
(loadData,
loadMatrix,
loadMatrixf,
loadUirs,
loadUirs2,
loadirdict,
loaduidict,
ypath,
xpath,
thetapath,
xrpath,
yrpath,
thetarpath,
rpath,
rrpath,
testsetpath,
filepath,
bu,
bi,
averagerating,
UserItemRating,
User,
Item,
IRdict,
URdict,
UIdict
)
where

-- | movielensData-script contains transformation of the initial data.
-- this scipt is mainly for testing purposes.

import System.IO
import Data.Matrix

import qualified Data.MultiMap as Map
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
--import Math.Statistics (mean)

type UserItemRating = [Int]
type User = Int
type Item = Int
type Rating = Int
type URdict = Map.MultiMap User Int
type IRdict = Map.MultiMap Item Int
type UIdict = Map.MultiMap User Item
type IUdict = Map.MultiMap Item User

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
testsetpath = "/home/lukas/oschena/ml-100k/500.test"

loadData::FilePath -> IO [[Float]]
loadData x = do
         contents <- readFile x
         return $ strs2fs $ lines contents

loadUirs :: FilePath -> IO [UserItemRating]
loadUirs fp = do 
  c <- readFile fp
  return $ strs2ints $ lines c

loadUirs2 :: FilePath -> IO [UserItemRating]
loadUirs2 f = do
  c <- BS.readFile f
  return $ map (\x -> f2 x) (BS.lines c)

loadMatrix :: FilePath -> IO (Matrix Float)
loadMatrix x = do
  contents <- readFile x
  return $ fromLists (strs2fs $ lines contents)


f2 :: BS.ByteString -> UserItemRating
f2 bs = map (\x -> f3 x) ( BS.words bs )

f3 :: BS.ByteString -> Int
f3 x = l4 (BS.readInt x)

l3 :: Maybe (Int, BS.ByteString) -> Float
l3 Nothing = error "readInt failed"
l3 (Just (x, _)) = fromIntegral x

l4 :: Maybe (Int, BS.ByteString) -> Int
l4 Nothing = error "readInt faild in l4"
l4 (Just (x, _)) = x
                   
l2 :: BS.ByteString -> Float
l2 x = l3 (BS.readInt x)

loadMatrixf :: FilePath -> IO (Matrix Int)
loadMatrixf x = do
  contents <- readFile x
  return $ fromLists (strs2ints2 $ lines contents)

strs2ints2 :: [String] -> [[Int]]
strs2ints2 xs = [str2int2 x | x <- xs]

strs2fs :: [String] -> [[Float]]
strs2fs xs = [str2fs x | x <- xs]

strs2ints :: [String] -> [UserItemRating]
strs2ints xs = [str2ints str| str <-  xs]

str2ints :: String -> [Int]
str2ints str = [read x | x <- strxs2 str]

strxs2 :: String -> [String]
strxs2 str = reverse $ tail $ reverse $ [""]

str2fs ::  String -> [Float]
str2fs str =  [read x | x <- strxs str ]

str2int2 :: String -> [Int]
str2int2 str = [read x | x <- strxs str]

strxs :: String -> [String]
strxs str = tail $ [""]

strs2texts :: [String] -> [[Int]]
strs2texts strlist = [init $ (texts2ints.str2text) x | x <- strlist ]

str2text :: String -> [String]
str2text str = [""]

texts2ints :: [String] -> [Int]
texts2ints ts = [ 1 ]

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

loadirdict :: [UserItemRating] -> IRdict
loadirdict uirs = foldl (\acc (u:i:r:_) -> Map.insert i r acc) Map.empty uirs

loaduidict :: [UserItemRating] -> Map.MultiMap User Rating
loaduidict = foldl (\acc (u:i:r:_) -> Map.insert u i acc) Map.empty 

--loaduirdict :: [UserItemRating] -> M.Map User Map.MultiMap Item Rating
--loaduirdict = foldl (\acc (u:i:r:_) -> Map.insert

insertir :: IRdict -> UserItemRating -> IRdict
insertir acc (u:i:r:_) = Map.insert i r acc

bu :: User -> Item -> [UserItemRating] -> Float
bu u _ r = fromIntegral((sum (ratingsOfUser u d))) / fromIntegral ((length (ratingsOfUser u d)))
         where d = loadurdict r

ratingsOfUser :: User -> URdict -> [Int]
ratingsOfUser u d = Map.lookup u d

uirsv :: [UserItemRating]
uirsv =  [[1,5,1],[1,5,2],[1,5,3]]

dv = loadurdict uirsv

averagerating :: [UserItemRating] -> Float
averagerating uirs = fromIntegral (sum [r|(u:i:r:_) <- uirs]) / fromIntegral (length uirs)

bi :: User -> Item -> IRdict -> Float
bi _ i d = fromIntegral (sum $ Map.lookup i d) / fromIntegral (length l)
  where l = Map.lookup i d

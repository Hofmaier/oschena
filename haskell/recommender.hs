import Data.Text (pack, unpack, split, Text)
import System.IO

main = do
     contents <- readFile "../ml-100k/u1.base"
     putStrLn "end of main"

filepath::String
filepath = "../ml-100k/u1.base"

loadData::FilePath -> IO [[Int]]
loadData x = do
         contents <- readFile x
         return $ strs2texts $ lines contents

firstline::String
firstline = "1\t1\t5\t874965758"

threelines :: [String]
threelines = ["1\t1\t5\t874965758","1\t2\t3\t876893171","1\t3\t4\t878542960"]

user::Int
user = 1

strs2texts :: [String] -> [[Int]]
strs2texts strlist = [init $ (texts2ints.str2text) x | x <- strlist ]

str2text :: String -> [Text]
str2text str = split (=='\t') (pack str)

texts2ints :: [Text] -> [Int]
texts2ints ts = [ read $ unpack x | x <- ts ]

sim_distance :: Int -> Int -> [[Int]] -> Float
sim_distance u1 u2 r = 1

shareditems :: Int -> Int -> [[Int]] -> [Int]
shareditems u1 u2 r = []

itemsOfUser :: Int -> [[Int]] -> [Int]
itemsOfUser user ratings = [i | u:i:xs <- ratings, u == user ]

shared :: [Int] -> [Int] -> [Int]
shared l1 l2 = [x| x <- l1, y <- l2, x == y]

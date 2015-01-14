import Data.Text (pack, unpack, split, Text)
import System.IO
import Math.Statistics

main = do
     contents <- readFile "/home/lukas/oschena/ml-100k/u1.base"
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

u1::Int
u1 = 1

u2 :: Int
u2 = 2

i1 = [2,3,4,5,5]
i2 = [1,2,3,4,4]

strs2texts :: [String] -> [[Int]]
strs2texts strlist = [init $ (texts2ints.str2text) x | x <- strlist ]

str2text :: String -> [Text]
str2text str = split (=='\t') (pack str)

texts2ints :: [Text] -> [Int]
texts2ints ts = [ read $ unpack x | x <- ts ]

sim_distance :: Int -> Int -> [[Int]] -> Float
sim_distance u1 u2 r = pearson (il2fl itemsu1) (il2fl itemsu2) 
                       where itemsu1 = ratingsOfUser u1 r (shareditems u1 u2 r)
                             itemsu2 = ratingsOfUser u2 r (shareditems u1 u2 r)


shareditems :: Int -> Int -> [[Int]] -> [Int]
shareditems u1 u2 r = shared (itemsOfUser u1 r) (itemsOfUser u2 r)

itemsOfUser :: Int -> [[Int]] -> [Int]
itemsOfUser user ratings = [i | u:i:xs <- ratings, u == user ]

shared :: [Int] -> [Int] -> [Int]
shared l1 l2 = [x| x <- l1, y <- l2, x == y]

ratingsOfUser :: Int -> [[Int]] -> [Int] -> [Int]
ratingsOfUser user ratings items = [r | u:i:r:_ <- ratings, u == user && elem i items]

il2fl::[Int] -> [Float]
il2fl il = [fromIntegral x | x <- il]


edist :: [Int] -> [Int] -> Float
edist r1 r2 = sqrt (sum [(fromIntegral a-fromIntegral b)^2|(a,b) <- zip r1 r2])

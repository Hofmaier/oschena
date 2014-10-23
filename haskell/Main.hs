import Useruser

main = do
     ts <- loadData filepath 
     putStrLn ("similarity is: " ++ (show (sim_distance u1 u2 ts)))

filepath :: String
filepath = "/home/lukas/oschena/ml-100k/u1.base"

loadData::FilePath -> IO Ratings
loadData x = do
         contents <- readFile x
         return $ strs2texts $ lines contents

firstline::String
firstline = "1\t1\t5\t874965758"

threelines :: [String]
threelines = ["1\t1\t5\t874965758","1\t2\t3\t876893171","1\t3\t4\t878542960"]

item :: Int
item = 1

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

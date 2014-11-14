module Useruser
(uupredict
) where

import Math.Statistics
import Data.List
import Data.Text as T (pack, unpack, split, Text) 

type Ratings = [[Int]]
type User = Int
type Item = Int

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

sim_distance :: Int -> Int -> [[Int]] -> Float
sim_distance u1 u2 r = pearson (il2fl itemsu1) (il2fl itemsu2) 
                       where itemsu1 = ratingsOfUser u1 r (shareditems u1 u2 r)
                             itemsu2 = ratingsOfUser u2 r (shareditems u1 u2 r)

uupredict :: User -> Item -> Ratings -> Float
uupredict u i ts = sum [s * (rating u i ts) | (s, u) <- fn] / sum [abs(s) | (s, u) <- fn]
  where fn = [(s, u) | (s, u) <- neighborhood u ts, hasrated i ts u]

hasrated :: Int -> Ratings -> Int -> Bool
hasrated item ts user = elem item (itemsOfUser user ts)

rating :: Int -> Int -> Ratings -> Float
rating user item ts = fromIntegral (head ( [r | u:i:r:_ <- ts, i == item && u == user]))

neighborhood :: Int -> Ratings -> [(Float, Int)]
neighborhood user ts = take 50 [(sim_distance user u ts, u) | u <- users ts]

users :: Ratings -> [Int]
users ts = nub [u | u:xs <- ts]
                                                              
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

module Useruser
(uupredict,
uupredict2,
RVec
) where

import Math.Statistics as MS
import Data.List
import Data.Text as T (pack, unpack, split, Text) 
import MovieLensData as Ml
import qualified Data.MultiMap as Map
import qualified Data.Map as M
import Data.Matrix
import qualified Data.Vector as V

type Ratings = [[Int]]

type RVec = V.Vector (Int, Int, Double)
type SimDict = M.Map User (M.Map User Double)
type IRDict = M.Map Item Double
type UIRDict = M.Map User (M.Map Item Double)
type UIDict = Map.MultiMap User Item
type Similarity = Double

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

sim_distance :: Int -> Int -> [[Int]] -> UIdict -> Matrix Int -> Float
sim_distance u1 u2 r d  m = edist itemsu1 itemsu2 
                       where itemsu1 = ratingsOfUser u1 m (shareditems u1 u2 d)
                             itemsu2 = ratingsOfUser u2 m (shareditems u1 u2 d)

ratingsOfUser :: Int -> Matrix Int -> [Int] -> [Int]
ratingsOfUser user ratings items = foldl (\acc i -> (getElem i user ratings):acc) [] items

uupredict :: User -> Item -> Ratings -> UIdict -> Matrix Int -> Float
uupredict u i ts d m = sum [s * rating u i ts | (s, u) <- fn u i ts d m] / f1 u i ts d m

uupredict2 :: User -> Item -> RVec -> Double
uupredict2 u i v =  n7 u i v
--  where n = Map.lookup i $ iudict v
--neighborhood2 :: User -> Item 
--neighborhood2 u i = 

f1 :: User -> Item -> Ratings -> UIdict -> Matrix Int -> Float
f1 u i ts d m = sum [abs(s) | (s, u) <- (fn u i ts d m)]

fn :: User -> Item -> [UserItemRating] -> UIdict -> Matrix Int -> [(Float, User)]
fn user i uirs d m = [(s, u) | (s, u) <- neighborhood user uirs d m, hasrated i d u]

hasrated :: Int -> UIdict -> Int -> Bool
hasrated item ts user = elem item (itemsOfUser user ts)

rating :: Int -> Int -> Ratings -> Float
rating user item ts = fromIntegral (head ( [r | u:i:r:_ <- ts, i == item && u == user]))

neighborhood :: Int -> Ratings -> UIdict -> Matrix Int -> [(Float, Int)]
neighborhood user ts d m =  [(sim_distance user u ts d m, u) | u <- users ts]

users :: Ratings -> [Int]
users ts = nub [u | u:xs <- ts]
                                                              
shareditems :: Int -> Int -> UIdict -> [Int]
shareditems u1 u2 r = shared (itemsOfUser u1 r) (itemsOfUser u2 r)

itemsOfUser :: Int -> Map.MultiMap User Item -> [Item]
itemsOfUser user d = Map.lookup user d

shared :: [Int] -> [Int] -> [Int]
shared l1 l2 = [x| x <- l1, y <- l2, x == y]

il2fl::[Int] -> [Float]
il2fl il = [fromIntegral x | x <- il]

uirsv :: [[Int]]
uirsv = [[1,1,5],[1,2,3],[1,3,4],[1,4,3],[1,5,3],[1,7,4],[1,8,1],[1,9,5],[1,11,2],[1,13,5]]

testset = [[1,6,5],[1,10,3],[1,12,5],[1,14,5],[1,17,3],[1,20,4],[1,23,4],[1,24,3],[1,27,2],[1,31,3]]

edist :: [Int] -> [Int] -> Float
edist r1 r2 = sqrt (sum [(fromIntegral a-fromIntegral b)^2|(a,b) <- zip r1 r2])

edist2 ::  [Double] -> [Double] -> Double
edist2 r1 r2 = sqrt (sum [( a - b )^2 | (a,b) <- zip r1 r2])

pdist :: [Double] -> [Double] -> Double
pdist x y = MS.pearson x y

iudict :: RVec -> Map.MultiMap Item User
iudict v = V.foldl (\acc (u,i,r) -> Map.insert i u acc) Map.empty v

uirdict :: RVec -> M.Map User (M.Map Item Double)
uirdict v = V.foldl ins2 M.empty v

ins2 :: M.Map User IRDict -> (User,Item,Double) -> M.Map User IRDict
ins2 acc (u, i, r) = M.insertWith (ins3 i r) u (M.singleton i r) acc

ins3 :: Item -> Double -> IRDict -> IRDict -> IRDict
ins3 i r _ old = M.insert i r old

uidict :: RVec -> Map.MultiMap User Item
uidict v = V.foldl (\acc (u,i,r) -> Map.insert u i acc) Map.empty v

--simap :: RVec -> SimDict
--simap v = V.foldl (\acc (u,i,r) -> u

--sm2 :: User -> RVec -> M.Map User Double
--sm2 u v = V.foldl (\acc (u,s) -> M.insert u s) Map.empty $ n1 u v

n4 :: [User] -> M.Map User Similarity -> UIRDict -> Item -> [(Similarity, Double, User)]
n4 xs m uirm i = map (n6 m uirm i) xs

n6 :: M.Map User Similarity -> UIRDict -> Item -> User -> (Similarity, Double, User)
n6 usm uirm i u = (M.findWithDefault (-1) u usm , s5 uirm u i, u)

n3 :: User -> Item -> RVec -> [(Similarity, Double, User)]
n3 u i v = n4 userratedi similarities uirmap i
           where uirmap = uirdict v
                 userratedi = Map.lookup i (iudict v)
                 similarities = n2 u v uirmap
  
n2 :: User -> RVec -> UIRDict -> M.Map User Similarity
n2 u v uirmap = foldl (\acc x -> M.insert x (s6 u uimap uirmap x) acc) M.empty users
  where users = Map.keys uimap
        uimap = uidict v

n7 :: User -> Item -> RVec -> Double
n7 u i v = sum [s * r | (s, r, u) <- neighborhood] / sum [ s | (s, _, _) <- neighborhood]
           where neighborhood = n3 u i v

n1 :: User -> RVec -> [Double]
n1 u v =  map (s6 u uimap uirmap) users
  where users = Map.keys uimap
        uimap = uidict v
        uirmap = uirdict v

testsimilarity u1 u2 v = (s1 u1 (shareditems u1 u2 uim) uirm, s1 u2 (shareditems u1 u2 uim) uirm)
                         where uim = uidict v
                               uirm = uirdict v

s6 :: User -> UIDict -> UIRDict -> User -> Double
s6 u m uirm ou = s7 ratingsuser ratingsotheruser
               where ratingsuser = s1 u si uirm
                     ratingsotheruser = s1 ou si uirm
                     si = shareditems u ou m

s7 :: [Double] -> [Double] -> Double
s7 r1 r2
  | stddev r1 == 0.0 || stddev r1 == 0.0 = edist2 r1 r2
  |  otherwise = MS.pearson r1 r2
  

s1 :: User -> [Item] -> UIRDict -> [Double]
s1 u xs m = foldl (s3 u m) [] xs

s3 :: User -> UIRDict -> [Double] -> Item -> [Double]
s3 u m acc i = s5 m u i : acc

s4 :: User -> Item -> UIRDict -> Maybe Double
s4 u i d = case M.lookup u d of
  Just sd -> M.lookup i sd
  Nothing -> error ("user not in uirdict. user: " ++ (show u))

s5 :: UIRDict -> User -> Item -> Double
s5 d u i = case s4 u i d of
  Just x -> x
  Nothing -> error ("item not in uirdict of user " ++ (show i))

sim :: User -> User -> (M.Map User (M.Map User Double)) -> Maybe Double
sim u ou d = case M.lookup u d of
  Just sd -> M.lookup ou sd
  Nothing -> error "user not in dict"

s2 :: User -> User -> (M.Map User (M.Map User Double)) -> Double
s2 u ou d = case sim u ou d of
  Just x -> x
  Nothing -> error "user not in dict"


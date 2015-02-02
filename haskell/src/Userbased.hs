module Userbased
(model, predict) where

import Math.Statistics as MS
import Data.List
import MovieLensData as Ml
import qualified Data.MultiMap as MM
import qualified Data.Map as M
import Data.Matrix
import qualified Data.Vector as V

type Ratings = [[Int]]

type RVec = V.Vector (Int, Int, Double)
type SimDict = M.Map User (M.Map User Double)
type IRDict = M.Map Item Double
type UIRDict = M.Map User (M.Map Item Double)
type UIDict = MM.MultiMap User Item
type Similarity = Double
type UUSDict = M.Map User (M.Map User Similarity)
type IUDict = MM.MultiMap Item User
type Model = (UIDict, UIRDict, IUDict, UUSDict, MM.MultiMap User Double )

-- | Given two user shareditems return all shared items of the given users
shareditems :: Int -> Int -> UIdict -> [Int]
shareditems u1 u2 r = shared (itemsOfUser u1 r) (itemsOfUser u2 r)
  where shared l1 l2 = [x| x <- l1, y <- l2, x == y]

-- | returns all items of user
itemsOfUser :: Int -> MM.MultiMap User Item -> [Item]
itemsOfUser user d = MM.lookup user d

-- | create mapping of item to all ratings of item
iudict :: RVec -> MM.MultiMap Item User
iudict v = V.foldl (\acc (u,i,r) -> MM.insert i u acc) MM.empty v

-- | creates mapping for user -item to rating
uirdict :: V.Vector (Int, Int, Double)
           -> M.Map User (M.Map Item Double)
uirdict v = V.foldl insertuser M.empty v
            where insertuser acc (u, i, r) = M.insertWith (insertitem i r) u (M.singleton i r) acc
                  insertitem i r _ old = M.insert i r old

-- | creates mapping for user to all items the user rated
uidict :: RVec -> MM.MultiMap User Item
uidict v = V.foldl (\acc (u,i,r) -> MM.insert u i acc) MM.empty v

-- | Create a neighborhood for every user u
neighborhoods :: [User] -- ^ Users
   -> UIDict
   -> UIRDict
   -> MM.MultiMap User Double
   -> M.Map User (M.Map User Similarity)
neighborhoods ux uim uirm urm = foldl (\acc u -> M.insert u (neighbors u) acc) M.empty ux
  where neighbors u = foldl (\acc o -> M.insert o (sim2 u o) acc) M.empty ux
        sim2 u o = pearson2 (rsi u (si u o)) (rsi o (si u o)) (ru u) (ru o)
        si u ou = shareditems u ou uim
        rsi u s = ratingsi s (uid u)
        uid u = M.findWithDefault M.empty u uirm
        ru u = MS.mean $ MM.lookup u urm
                           
-- | Given a list of item and a list a map with all ratings of a user returns
-- the ratings for these items. If items not in the map return 0
ratingsi :: [Item]
         -> M.Map Item Double
         -> [Double]
ratingsi xs m = foldl (\acc x -> (M.findWithDefault 0 x m) :acc) [] xs

-- | Computes the similarity between two list of ratings
pearson2 :: [Double] -- ^ ratings of user 1
         -> [Double] -- ^ ratings of user 2
         -> Double -- ^ mean of all ratings of user 1
         -> Double -- ^ mean of all ratings of user 2
         -> Double -- ^ similarity
pearson2 r1 r2 mr1 mr2 = n / d
  where n = sum (zipWith (\x y -> x*y) (dev r1 mr1) (dev r2 mr2))
        dev xs m = map (\x -> x - m ) xs
        d = (dev2 r1 mr1) * (dev2 r2 mr2)
        dev2 xs m = sqrt (sum (map (\x -> (x-m)^2) xs))
                             
-- | comutes a prediction for rating of user u on item i based on
-- the neighorhood of u
predict :: Model -- ^ knn model
        -> Int -- ^ User
        -> Int -- ^ Item
        -> Double -- ^ rating prediction
predict m u i = (ru u) + (sum [(s * (r - (ru u))) | (s, r, u) <- neighborhood] / sum [ s | (s, _, _) <- neighborhood])
  where neighborhood = nearest u i (uim,uirm,ium,uusm)
        (uim,uirm,ium,uusm,urm) = m
        ru u = MS.mean $ MM.lookup u urm
        dev u = stddev $ MM.lookup u urm

-- | return the nearest k neighbors who rated i
nearest :: User
        -> Item
        -> (UIDict, UIRDict, IUDict, UUSDict)
        -> [(Double, Double, User)]
nearest u i (_, uirm, ium, uusm) = take 20 (reverse (sort (neighbors u i)))
  where neighbors u i = neighborsForItem userratedi simMap uirm i
        userratedi = MM.lookup i ium
        simMap = M.findWithDefault M.empty u uusm

-- | for all users who rated item i return the similarity and the rating for item i
neighborsForItem :: [User]
                 -> M.Map User Similarity
                 -> UIRDict
                 -> Item
                 -> [(Similarity, Double, User)]
neighborsForItem userratedi usm uirm i = map usersim userratedi
    where usersim ou = (M.findWithDefault 0 ou usm , rating ou, ou)
          rating ou = M.findWithDefault 0 i (uid ou)
          uid ou = M.findWithDefault M.empty ou uirm

-- | generates model for userbase collaborative filtering
model :: RVec
         -> (UIDict, UIRDict, IUDict, UUSDict, MM.MultiMap User Double)
model v = (uim, uirmap, ium, uusm, urm)
  where uirmap = uirdict v
        ium = iudict v
        uusm = neighborhoods users uim uirmap urm
        uim = uidict v
        users = MM.keys uim
        urm = V.foldl (\acc (u,_,r) -> MM.insert u r acc) MM.empty v

-- | computes user rating bias 
bu :: Double
   -> MM.MultiMap Int Double
   -> User
   -> Double
bu mu urm u = bu
  where ratingsofUser = MM.lookup u urm
        dev = map (\x -> x - mu) $ ratingsofUser
        s = sum dev 
        n = fromIntegral $ length ratingsofUser
        bu = s / n

-- | comutes eucledian similarity between user
sim_distance :: User 
             -> User
             -> [[Int]]
             -> UIdict
             -> Matrix Int
             -> Float
sim_distance u1 u2 r d  m = edist itemsu1 itemsu2 
  where itemsu1 = ratingsOfUser u1 m (shareditems u1 u2 d)
        itemsu2 = ratingsOfUser u2 m (shareditems u1 u2 d)

ratingsOfUser :: Int -> Matrix Int -> [Int] -> [Int]
ratingsOfUser user ratings items = foldl (\acc i -> (getElem i user ratings):acc) [] items

hasrated :: Int -> UIdict -> Int -> Bool
hasrated item ts user = elem item (itemsOfUser user ts)

rating :: Int -> Int -> Ratings -> Float
rating user item ts = fromIntegral (head ( [r | u:i:r:_ <- ts, i == item && u == user]))

neighborhood :: Int -> Ratings -> UIdict -> Matrix Int -> [(Float, Int)]
neighborhood user ts d m =  [(sim_distance user u ts d m, u) | u <- users ts]

-- | get all users. no duplicates
users :: Ratings -> [Int]
users ts = nub [u | u:xs <- ts]

-- | euclidian distance between two user. takes shared ratings as paramaters
edist :: [Int] -> [Int] -> Float
edist r1 r2 = sqrt (sum [(fromIntegral a-fromIntegral b)^2|(a,b) <- zip r1 r2])

-- | euclidian distance between two user. takes shared ratings as paramaters
edist2 ::  [Double] -> [Double] -> Double
edist2 r1 r2 = 1 - (sqrt (sum [( a - b )^2 | (a,b) <- zip r1 r2]))

pdist :: [Double] -> [Double] -> Double
pdist x y = MS.pearson x y

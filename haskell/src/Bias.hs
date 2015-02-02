module Bias (model, predict) where

import qualified Data.MultiMap as MM
import qualified Data.Map as M
import qualified Data.Vector as V

type Item = Int
type User = Int

-- | creates dictionaries for unpersonalized recommendation.
model :: V.Vector (Int, Int, Double)
       -> (Double, MM.MultiMap User Double, MM.MultiMap Item User, M.Map User (M.Map Item Double))
model v =  (mu, urm, ium, uirmap)
  where mu = (V.sum $ V.map (\(u,i,r)->r) v) / (fromIntegral $ V.length v)
        urm = V.foldl (\acc (u,_,r) -> MM.insert u r acc) MM.empty v
        ium = iudict v
        uirmap = uirdict v

iudict :: V.Vector (Int, Int, Double)  
       -> MM.MultiMap Item User
iudict v = V.foldl (\acc (u,i,r) -> MM.insert i u acc) MM.empty v


-- | creates mapping for user -item to rating
uirdict :: V.Vector (Int, Int, Double)
           -> M.Map User (M.Map Item Double)
uirdict v = V.foldl insertuser M.empty v
            where insertuser acc (u, i, r) = M.insertWith (insertitem i r) u (M.singleton i r) acc
                  insertitem i r _ old = M.insert i r old
                  
-- | Predicts the rating for user us and item it. takes model of Bias module
predict :: Double  -- ^ Average rating
          -> MM.MultiMap Int Double -- ^ map. keys are user. values are ratings of user
          -> MM.MultiMap Item User -- ^ map: keys are items. values are user how rated item
          -> M.Map User (M.Map Item Double) -- ^ map contains all ratings to user item keys
          -> Int   -- ^ User id
          -> Int   -- ^ Item id
          -> Double
predict mu urm ium uirm us it = mu + (bu mu urm us) + bi
  where dev = map (\x -> rating x it - (bu mu urm x) - mu) users
        s = sum $ dev 
        n = fromIntegral $ length users
        rating u i = rui u i (irm u)  
        userlength = fromIntegral $ length users
        bi = s / n
        rui u i m = M.findWithDefault 0 i m
        users = MM.lookup it ium
        irm u = M.findWithDefault M.empty u uirm

-- | Predicts user deviation from global average
bu :: Double
   -> MM.MultiMap Int Double -- ^ Map with User to Ratings mapping
   -> User
   -> Double
bu mu urm u = bu
  where ratingsofUser = MM.lookup u urm
        dev = sum (map (\x -> x - mu) $ ratingsofUser)
        bu = dev / (fromIntegral $ length ratingsofUser)

--bi = s / n
--  where dev = map (\x -> rating x it - (bu mu urm x) - mu) users

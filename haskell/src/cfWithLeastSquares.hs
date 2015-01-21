import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as Bl
import qualified Data.MultiMap as MM
import qualified Data.Map as M
import Data.Packed.Matrix as L
import Data.Packed.Vector as Lv
import Numeric.LinearAlgebra.Algorithms as Nla
import Numeric.LinearAlgebra

basefile = "base.csv"
testfile = "/home/lukas/oschena/ml-100k/test.csv"
nfeatures = 2
nitems = 1650
nusers = 943

-- this script is an antempt to solve the recommender problem with linear least quares
-- optimization.

main = do
  c1 <- Bl.readFile basefile
  c2 <- Bl.readFile testfile
  let either = decode NoHeader c1 :: Either String (V.Vector (Int, Int, Double))
  let teste = decode NoHeader c1 :: Either String (V.Vector (Int, Int, Double))
  let vec =  toVec either
  let testvec =  toVec either
  let id = dict insertitem vec
  let ud = dict insertuser vec
  let m = fillmat map2rating id
  let binm = fillmat map2bin id
  let ul = userratings 2 m
  let v =   L.toColumns m !! 1
  let a = (diam v ) <> initialm nitems
  let x = Nla.linearSolveSVD a (L.asColumn v)
  let pus = trans (f1 m (initialm nitems))
  let m1 = trans m
  let pis = trans (f1 m1 ( pus))
  putStrLn ("mae: " ++ (show $ (initialm nitems) @@> (0,1)))
  putStrLn ("mae: " ++ (show $ mae2 (predict (initialm nusers) (initialm nitems))  ( testvec)))
  putStrLn ("mae: " ++ (show $ mae2 (predict pus pis)  ( testvec)))
  let pus1 = trans (f1 m pis)
  let pis1 = trans (f1 m1 pus1)
  putStrLn "second run"

f1 :: L.Matrix Double -> L.Matrix Double -> L.Matrix Double
f1 m pus = L.fromBlocks [reverse (f2 m pus)]
f2 m pus = foldl (f3 pus) []  $ L.toColumns m
f3 pus acc pu = (Nla.linearSolveSVD (f4 pus pu) (L.asColumn pu)) : acc
f4 pus pu = diam pu <> pus

predict :: L.Matrix Double
        -> L.Matrix Double
        -> Int
        -> Int
        -> Double
predict pus pis u i = if i > 1650 then 3.5 else sum (map (\x -> (pus @@> (u-1, x)) * (it x)) [0..nfeatures-1])
  where it x = pis @@> (i-1, x)

iter3 :: L.Matrix Double -> L.Matrix Double
iter3 m = L.fromBlocks [iter2 m]
iter2 :: L.Matrix Double -> [L.Matrix Double]
iter2 m =  foldl iter [] (take 6 (L.toColumns m))
iter :: [L.Matrix Double] -> Lv.Vector Double -> [L.Matrix Double]
iter acc pu = (Nla.linearSolveSVD ((diam pu ) <> initialm 3) (L.asColumn pu)) : acc
amat pu = diam pu <> initialm 3
pu :: L.Matrix Double -> Int -> L.Matrix Double
pu m u = L.subMatrix (0, u) (1650, u) m

map2rating :: M.Map Int Double -> [Double]
map2rating map = foldl f [] [1..943]
  where f acc u = r u :acc
        r u = M.findWithDefault 0 u map

map2bin :: M.Map Int Double -> [Double]
map2bin map = foldl f [] [1..943]
  where f acc u = (if M.member u map then 1 else 0) : acc

fillmat :: (M.Map Int Double -> [Double])
           -> M.Map Int (M.Map Int Double)
           -> L.Matrix Double
fillmat f d = L.fromLists (map f $ M.elems d)

userratings :: Int 
            -> L.Matrix Double
            -> [Double]
userratings u m = head $ L.toLists $ L.trans $ L.subMatrix (0,u) (1650, u) m

diam :: Lv.Vector Double -> L.Matrix Double
diam v = L.diagRect 0.0 v l l
         where l = dim v

step1 :: L.Matrix Double 
     -> L.Matrix Double
     -> L.Matrix Double
step1 a pu = Nla.linearSolveLS a pu

initialm :: Int -> L.Matrix Double
initialm n = L.fromLists (featurelist n)

featurelist n = replicate n features
features = replicate nfeatures 0.1

w :: L.Matrix Double -> Int -> Lv.Vector Double
w m u = head $ L.toColumns m

v :: Lv.Vector Double
v = Lv.fromList [1.0, 0.0, 1.0]

diamat :: L.Matrix Double -> L.Matrix Double
diamat m = L.diagRect 0.0 (w m 1) 3 3

toVec :: Either String (V.Vector (Int, Int, Double))
      -> V.Vector (Int, Int, Double)
toVec (Left err) = error err
toVec (Right v) = v

dict :: (M.Map Int (M.Map Int Double) -> (Int, Int, Double) -> M.Map Int (M.Map Int Double))
        -> V.Vector (Int, Int, Double)
        -> M.Map Int (M.Map Int Double)
dict f v = V.foldl f M.empty v

insertuser :: M.Map Int (M.Map Int Double) 
          -> (Int, Int, Double) 
          -> M.Map Int (M.Map Int Double)
insertuser acc (u, i, r) = M.insertWith f u value acc
 where f new_value old_value = M.insert i r old_value
       value = M.singleton i r

insertitem :: M.Map Int (M.Map Int Double) 
          -> (Int, Int, Double) 
          -> M.Map Int (M.Map Int Double)
insertitem acc (u, i, r) = M.insertWith f i value acc
 where f new_value old_value = M.insert u r old_value
       value = M.singleton u r
       
mae2 :: (Int -> Int -> Double) 
     -> V.Vector (Int,Int,Double)
     -> Double
mae2 p v = (V.sum $ errors2 p v) / (fromIntegral (V.length v))

errors2 ::( Int -> Int -> Double )
       -> V.Vector (Int,Int,Double)
       -> V.Vector Double
errors2 p v = V.filter (\x -> not $ isNaN x) $ V.map (\(u, i, r) -> abs (r - (p u i))) v

mu :: [(Int,Int,Double)] -> Double
mu v = (sum $ map (\(u,i,r)->r) v) / (fromIntegral $ length v)

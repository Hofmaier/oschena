module Matrixfactorization (model, predict) where

import qualified Data.ByteString.Lazy as Bl
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U 
import qualified Data.Matrix as M
import qualified Data.MultiMap as MM
import qualified Data.Map as Map
import Data.Csv
import qualified Numeric.SGD as S
import qualified Numeric.SGD.Dataset as D
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))
type Parameter = ( M.Matrix Double, M.Matrix Double)
type Parameter2 = M.Matrix (V.Vector Double)
type Csvdata = Either String (V.Vector [Double])
type CsvInt = Either String (V.Vector [Int])
type Item = Int
type User = Int
type Features = M.Matrix (V.Vector Double)
type Rating = (Int, Int, Double)
type Elem = [Rating]

alpha = 0.15
lambda = 0.01
nr_iter = 2
nfeatures = 4
nusers = 943
nitems = 1682
xlength = nitems * nfeatures
thetalength = nusers * nfeatures
frange = [0..nfeatures-1]
datasetSize = 80000

runsgd :: S.Para
       -> MM.MultiMap Item User
       -> Map.Map User (Map.Map Item Double)
       -> [(Int,Int,Double)]
       -> D.Dataset (Int,Int,Double)
       -> IO S.Para
runsgd para ium uirm all d = do
  let sgdArgs = S.sgdArgsDefault { S.iterNum = nr_iter, S.gain0=alpha }
  let uim = uidict all
  let avg = mu all
  S.sgd sgdArgs (notify avg all) (grad2 avg) d para 

iudict :: V.Vector (Int,Int,Double) -> MM.MultiMap Item User
iudict v = V.foldl (\acc (u,i,r) -> MM.insert i u acc) MM.empty v

uidict :: [(Int,Int,Double)] -> MM.MultiMap Item User
uidict v = foldl (\acc (u,i,r) -> MM.insert u i acc) MM.empty v

uirdict :: V.Vector (Int,Int,Double) -> Map.Map User (Map.Map Item Double)
uirdict v = V.foldl ins2 Map.empty v

ins2 :: Map.Map User (Map.Map Item Double)
        -> (User, Item, Double)
        -> Map.Map User (Map.Map Item Double)
ins2 acc (u, i, r) = Map.insertWith (ins3 i r) u (Map.singleton i r) acc

ins3 :: Item
        -> Double
        -> (Map.Map Item Double)
        -> (Map.Map Item Double)
        -> (Map.Map Item Double)
ins3 i r _ old = Map.insert i r old

goal :: Double -> S.Para -> [(Int,Int,Double)] -> Double
goal mu para xs =  sum (map (\x -> err2 x) xs)
                where err2 (u, i, r) = (r - mu - (sum [el i u f | f <- frange]))^2
                      el i u f = para U.! (index2 i f) * (para U.! (nusers+(index2 u f)))
        

grad :: MM.MultiMap Item User
        -> MM.MultiMap Int User
        -> Map.Map User (Map.Map Item Double)
        -> S.Para
        -> (Int,Int,Double)
        -> S.Grad
grad ium uim uirm para (user,item, rating) = S.fromList (dims ++ dims2) 
  where dims =  (foldl items [] frange )
        dims2 = foldl users [] frange
        items acc f = [(index2 item f, g item f)] ++ acc
        users acc f = [(index3 user f, g2 user f)] ++ acc
        g i f = sum [f1 u i (getr u i) f | u <- MM.lookup i ium]
        g2 u f = sum [f4 u i (getr u i) f | i <- MM.lookup u uim]
        getr us it = Map.findWithDefault 2.5 it (Map.findWithDefault Map.empty us uirm )
        f1 u i r f = (f2 u i r) * para U.! (index3 u f)
        f4 u i r f = (f2 u i r) * para U.! (index2 i f)
        f2 u i r = f3 u i - r
        f3 u i = sum (map (\x -> para U.! (index3 u x) * (para U.! (index2 i x))) frange )

grad2 :: Double
      -> S.Para 
      -> (Int,Int,Double)
      -> S.Grad
grad2 mu para (u,i,r) = S.fromList f1 
  where f1 = (qi u i r) ++ (pu u i r)
        qi u i r= qi2 u i (eui3 mu u i para r) 
        qi2 u i e =  [(index2 i f, (para U.! (index3 u f)) * e - (lambda *(para U.! (index2 i f))) )| f <- frange]
        pu u i r = pu2 u i (eui3 mu u i para r)
        pu2 u i e =  [(index3 u f, e * (para U.! (index2 i f)) - (lambda*(para U.! (index3 u f)))) | f <- frange]

grad3 :: S.Para
      -> (Int,Int,Double)
      -> S.Grad
grad3 para (user,item,r) = S.fromList [(0, f user),(1, f item)]
  where f = fromIntegral

negGrad :: (S.Para -> (Int,Int,Double) -> S.Grad)
        -> (S.Para -> (Int,Int,Double) -> S.Grad)
negGrad g para x = fmap negate (g para x)

el :: S.Para -> Int -> Int -> Int -> Double
el para i u f =  para U.! (index2 i f) * (para U.! (nusers+(index2 u f)))

index2 :: Int -> Int -> Int
index2 x f = (x-1) * nfeatures + f

index3 x f = nitems + nfeatures * (x-1) + f

t1 :: Bl.ByteString -> M.Matrix Double
t1 bstr = toMatrix ( decode NoHeader bstr :: Csvdata)

t2 :: Bl.ByteString -> M.Matrix (V.Vector Double)
t2 bstr = toMatrix2 ( decode NoHeader bstr :: Csvdata )

t3 :: Bl.ByteString -> V.Vector (Int,Int,Double)
t3 bstr = toVec (decode NoHeader bstr :: Either String (V.Vector (Int,Int,Double)))

toVec :: Either String (V.Vector (Int,Int,Double))
      -> V.Vector Rating
toVec (Left err) = error err
toVec (Right v) = v

toList :: CsvInt -> [[Int]]
toList (Right v) = V.toList v
toList (Left v) = [[]]

toMatrix :: Csvdata -> M.Matrix Double
toMatrix (Right v) = M.fromLists $ V.toList v
toMatrix (Left str) = M.fromList 0 0 []

toMatrix2 :: Csvdata -> M.Matrix (V.Vector Double)
toMatrix2 (Right v) = M.fromList (V.length v) 1 $ map (\x -> V.fromList x) $ V.toList v
toMatrix2 (Left v) = M.fromList 1 1 [ V.empty ]

dim :: M.Matrix a -> String
dim m = (show $ M.nrows m) ++ ", " ++ (show $ M.ncols m)

cost :: M.Matrix Double -> Parameter -> Double
cost y p =  f5 $ errors y p

cost2 :: M.Matrix Double -> (Features, Features) -> Double
cost2 y (x,theta) = sum $ map (\(i,u) -> eui2 (rui (i,u) y) (M.getElem i 1 x) (M.getElem u 1 theta)) $ trainingcases y

f5 :: [(Double,Double)] -> Double
f5 es = sum [r^2|(r,_) <- (filter (\(x,y) -> y /= 0.0) es)]

errors :: M.Matrix Double -> Parameter -> [(Double, Double)]
errors y p = zipWith (\r p1 -> (r - p1, r)) (M.toList y) (M.toList $ multiplym p)

multiplym :: Parameter -> M.Matrix Double
multiplym (pi, pu) = M.multStd pi (M.transpose pu)

trainingcases :: M.Matrix Double -> [(Int, Int)]
trainingcases y = [(x2, x1) | x1 <- [1..(M.ncols y)], x2 <- [1..(M.nrows y)], (M.getElem x2 x1 y) /= 0.0 ]

f7 y p = foldl (\acc x -> f6 y acc) p [1..nr_iter]

f3 y p = foldl (\acc x -> step acc x y) p $ trainingcases y
f6 y p = foldl (\acc x -> step2 acc x y) p $ trainingcases y

step :: Parameter -> (Int,Int) -> M.Matrix Double -> Parameter
step (x, theta) (i, u) y = (updateRow i newqi x, updateRow u newqu theta)
                             where newqi = f2 oldqi oldpu error
                                   newqu = f2 oldpu oldqi error
                                   oldqi = getRow i x
                                   oldpu = getRow u theta
                                   error = eui y (x,theta) (i,u) 

step2 :: (Features, Features) -> (Int,Int) -> M.Matrix Double -> (Features, Features)
step2 (x, theta) (i, u) y = (updateRow2 i newqi x, updateRow2 u newpu theta)
                             where newqi = f4 oldqi oldpu error
                                   newpu = f4 oldpu oldqi error
                                   oldqi = M.getElem i 1 x
                                   oldpu = M.getElem u 1 theta
                                   error = eui2 (rui (i,u) y) oldqi oldpu

computeqi :: M.Matrix Double -> Parameter -> (Item, User) -> M.Matrix Double
computeqi y p@(x,theta) iu@(i,u) = updateRow i newqi x
                where newqi = f2 oldqi oldpu error
                      oldqi = getRow i x
                      oldpu = getRow u theta
                      error = eui y p iu

f2 :: M.Matrix Double -> M.Matrix Double -> Double -> M.Matrix Double
f2 qi pu e = M.elementwise (\a b -> a+b) qi $ M.scaleMatrix alpha $ M.scaleMatrix e pu

f4 :: V.Vector Double
  -> V.Vector Double
  -> Double
  -> V.Vector Double
f4 qi pu e = V.zipWith (+) qi (V.map (alpha*) direction)
                               where lambdaqi = V.map (lambda*) qi
                                     epu = V.map (e*) pu
                                     direction = V.zipWith (-) epu lambdaqi
                                     
eui :: M.Matrix Double -> Parameter -> (Item, User) -> Double
eui y (x, theta) (i,u) = rui (i,u) y - M.getElem 1 1 (M.multStd qi $ M.transpose pu)
                         where qi = getRow i x
                               pu = getRow u theta

eui2 :: Double -> V.Vector Double -> V.Vector Double -> Double
eui2 r qi pu = r - predict qi pu

eui3 :: Double
     -> Int
     -> Int
     -> U.Vector Double
     -> Double
     -> Double
eui3 mu u i para r = r - mu - (sum [(para U.! (index2 i f)) * (para U.! (index3 u f))| f <- frange])

predict :: V.Vector Double -> V.Vector Double -> Double
predict qi pu =  V.sum $ V.zipWith (*) qi pu

rui :: (Item, User) -> M.Matrix Double -> Double
rui (i,u) y = M.getElem i u y

pi :: Item -> M.Matrix Double -> M.Matrix Double
pi i x = getRow i x

getRow :: Int -> M.Matrix Double -> M.Matrix Double
getRow r m = M.submatrix r r 1 (M.ncols m) m

pu u theta = getRow u theta

updateRow :: Int -> M.Matrix Double -> M.Matrix Double -> M.Matrix Double
updateRow row v m = foldl (\acc x -> M.setElem (M.getElem 1 x v) (row, x)  acc ) m [1..(M.ncols v)]

updateRow2 :: Int -> V.Vector Double -> Features -> Features
updateRow2 r pi items = M.setElem pi (r,1) items

testmatrix = M.matrix 4 4 (\(i,j) -> 5.5)
testvector = V.generate 4 (\i -> 23.2)

tm = M.matrix 1 4 (\(i,j) -> 4.3)

--mae :: [[Int]] -> (Features, Features) -> Double
--mae test p = (sum $ map (\(u:i:r:_) -> p2 i u r p) test) / (fromIntegral $ length test)

p2 :: (Features, Features) -> Int -> Int -> Double
p2 (items, users) u i = predict (M.getElem u 1 users) (M.getElem i 1 items)

predict2 :: Double
         -> U.Vector Double
        -> Int
        -> Int
        -> Double
predict2 mu para u i = sum [mu - (para U.! (index2 i f)) * (para U.! (index3 u f))| f <- frange]

mae2 :: Double
     -> (Int -> Int -> Double) 
     -> V.Vector Rating
     -> Double
mae2 mu p v = (V.sum $ errors2 mu p v) / (fromIntegral (V.length v))

errors2 ::Double
        -> ( Int -> Int -> Double )
       -> V.Vector Rating
       -> V.Vector Double
errors2 mu p v = V.filter (\x -> not $ isNaN x) $ V.map (\(u, i, r) -> abs (r - mu - (p u i))) v

notify :: Double -> [(Int,Int,Double)] -> S.Para -> Int -> IO ()
notify mu d para k = putStr ("\n" ++ (show mu) ++ ("\t") ++ (show (goal mu para d)))

mu :: [(Int,Int,Double)] -> Double
mu v = (sum $ map (\(u,i,r)->r) v) / (fromIntegral $ length v)

main = do
    c1 <- Bl.readFile basefile
    c2 <- Bl.readFile testfile
    let dataset = t3 c1
    let testlist = t3 c2
    let ium = iudict dataset
    let uirm = uirdict dataset
    let npara = (nfeatures * nusers) + (nfeatures * nitems)
    let para = U.replicate npara (0.1::Double)
    let datalist = V.toList dataset
    let avg = mu datalist
    hSetBuffering stdout NoBuffering
    fitted <- D.withVect datalist (runsgd para ium uirm datalist)
    putStrLn ("start: " ++ (show (V.length dataset)))
    putStrLn $ "mean absolute error is: " ++ (show $ mae2 avg (predict2 avg fitted) testlist)

testfile = "/home/lukas/oschena/ml-100k/test.csv"
basefile = "/home/lukas/oschena/ml-100k/base.csv"

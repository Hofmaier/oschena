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

alpha = 0.01
lambda = 0.1
nr_iter = 3
nfeatures = 4
nusers = 943
nitems = 1682
xlength = nitems * nfeatures
thetalength = nusers * nfeatures
frange = [0..nfeatures-1]
datasetSize = 80000

-- | computes the latent factor vectors for matrixfactorixation
model :: V.Vector (Int,Int,Double) -> IO S.Para
model dataset = do 
    let ium = iudict dataset
    let uirm = uirdict dataset
    let npara = (nfeatures * nusers) + (nfeatures * nitems)
    let para = U.replicate npara (0.1::Double)
    D.withVect (V.toList dataset) (runsgd para ium uirm (V.toList dataset))

-- | comutes a prediction for rating of user u on item i based on
-- the latent factor vectors
predict :: S.Para -- ^ fitted latent factor vector in flat vector
        -> Int -- ^ User id
        -> Int -- ^ Item id
        -> Double
predict para u i = sum [(para U.! (indexi i f)) * (para U.! (indexu u f))| f <- frange]

-- | Run the monadic version of SGD.
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
  S.sgd sgdArgs (notify all) (grad) d para 

-- | Notification run by the sgdM function every parameters update.
notify :: [(Int,Int,Double)] -> S.Para -> Int -> IO ()
notify d para k = putStr ("\n" ++ (show k) ++ ("\t") ++ (show (goal para d)))

-- | One gradient descent step es descripe on
-- http://sifter.org/~simon/journal/2061211.html
grad :: S.Para 
      -> (Int,Int,Double)
      -> S.Grad
grad para (u,i,r) = S.fromList concatpara
  where concatpara = (qi u i r) ++ (pu u i r)
        qi u i r= qi2 u i (errorui u i para r) 
        qi2 u i e =  [(indexi i f, ((para U.! (indexu u f)) * e) - (lambda *(para U.! (index2 u f))))| f <- frange]
        pu u i r = pu2 u i (errorui u i para r)
        pu2 u i e =  [(indexu u f, ((para U.! (indexi i f)) * e) - (lambda *(para U.! (indexu u f) )))| f <- frange]

-- | calculates error for trainingdata user , item,
-- rating with a given latent factor vector
errorui :: Int -- ^ User id
     -> Int -- ^ Item id
     -> U.Vector Double -- ^ Paramter Vector
     -> Double -- ^ Rating
     -> Double -- ^ Error
errorui u i para r = r - (sum [(para U.! (indexi i f)) * (para U.! (indexu u f))| f <- frange])


-- | Get the index of a feature of a item in the parametervector
indexi :: Int -- ^ item nr
       -> Int -- ^ feature nr
       -> Int -- ^ Index for paramteter vector
indexi x f = ((x-1) * nfeatures) + f

-- | Get the index of a featuer of a user in the parametervector
indexu :: Int -- ^ user nr
       -> Int -- ^ feature nr
       -> Int -- ^ Index for paramteter vector
indexu x f = nitems + (nfeatures * (x-1)) + f

-- | cost function. return the squared sum of all error with training set
goal :: S.Para -> [(Int,Int,Double)] -> Double
goal para xs = sum (map (\x -> err2 x) xs)
                where err2 (u, i, r) = (r - (sum [el i u f | f <- frange]))^2
                      el i u f = para U.! (index2 i f) * (para U.! (nusers+(index2 u f)))

-- | negates all gradient parameters
negGrad :: (S.Para -> (Int,Int,Double) -> S.Grad)
        -> (S.Para -> (Int,Int,Double) -> S.Grad)
negGrad g para x = fmap negate (g para x)

-- | return a list with all trainingcases
trainingcases :: M.Matrix Double -> [(Int, Int)]
trainingcases y = [(x2, x1) | x1 <- [1..(M.ncols y)], x2 <- [1..(M.nrows y)], (M.getElem x2 x1 y) /= 0.0 ]

-- | fit latent factor vectors with funks sgd
fitlfv :: M.Matrix Double
       -> (Features, Features)
       -> (Features, Features)
fitlfv y p = foldl (\acc x -> iteration y acc) p [1..nr_iter]
  where iteration y p = foldl (\acc x -> step acc x y) p $ trainingcases y

-- | executes one update step
step :: (Features, Features)
      -> (Int,Int)
      -> M.Matrix Double
      -> (Features, Features)
step (x, theta) (i, u) y = (updateRow2 i newqi x, updateRow2 u newpu theta)
                             where newqi = updatelfv oldqi oldpu error
                                   newpu = updatelfv oldpu oldqi error
                                   oldqi = M.getElem i 1 x
                                   oldpu = M.getElem u 1 theta
                                   error = eui2 (rui (i,u) y) oldqi oldpu

-- | calculates error for two latent factor vector pi pu and a given rating
eui2 :: Double -> V.Vector Double -> V.Vector Double -> Double
eui2 r qi pu = r - predict2 qi pu

-- | update latent factor vector one step in direction of gradient
updatelfv :: V.Vector Double
  -> V.Vector Double
  -> Double
  -> V.Vector Double
updatelfv qi pu e = V.zipWith (+) qi (V.map (alpha*) direction)
                               where lambdaqi = V.map (lambda*) qi
                                     epu = V.map (e*) pu
                                     direction = V.zipWith (-) epu lambdaqi

-- | computes the error of the latent factor vectors, given user and item
eui :: M.Matrix Double -> Parameter -> (Item, User) -> Double
eui y (x, theta) (i,u) = rui (i,u) y - M.getElem 1 1 (M.multStd qi $ M.transpose pu)
                         where qi = getRow i x
                               pu = getRow u theta

-- | testmatrix
tm = M.matrix 1 4 (\(i,j) -> 4.3)
                  
-- | predict error for two latent factor vectors as Vectors
predict2 :: V.Vector Double -> V.Vector Double -> Double
predict2 qi pu =  V.sum $ V.zipWith (*) qi pu

rui :: (Item, User) -> M.Matrix Double -> Double
rui (i,u) y = M.getElem i u y

pi :: Item -> M.Matrix Double -> M.Matrix Double
pi i x = getRow i x

getRow :: Int -> M.Matrix Double -> M.Matrix Double
getRow r m = M.submatrix r r 1 (M.ncols m) m

pu :: User -> M.Matrix Double -> M.Matrix Double
pu u theta = getRow u theta

updateRow2 :: Int -> V.Vector Double -> Features -> Features
updateRow2 r pi items = M.setElem pi (r,1) items

p2 :: (Features, Features) -> Int -> Int -> Double
p2 (items, users) u i = predict2 (M.getElem u 1 users) (M.getElem i 1 items)

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


-- | computes the expecation value of all ratings mu
mu :: [(Int,Int,Double)] -> Double
mu v = (sum $ map (\(u,i,r)->r) v) / (fromIntegral $ length v)

-- | main for test purposes
main = do
    c1 <- Bl.readFile basefile
    c2 <- Bl.readFile testfile
    let dataset = bstr2vec c1
    let testlist = bstr2vec c2
    let ium = iudict dataset
    let uirm = uirdict dataset
    let npara = (nfeatures * nusers) + (nfeatures * nitems)
    let para = U.replicate npara (0.1::Double)
    let datalist = V.toList dataset
    let avg = mu datalist
    hSetBuffering stdout NoBuffering
    fitted <- D.withVect datalist (runsgd para ium uirm datalist)
    putStrLn ("start: " ++ (show (V.length dataset)))
    putStrLn $ "mean absolute error is: " ++ (show $ mae2 avg (predict fitted) testlist)

testfile = "/home/lukas/oschena/ml-100k/test.csv"
basefile = "/home/lukas/oschena/ml-100k/base.csv"

-- | testdata
testmatrix = M.matrix 4 4 (\(i,j) -> 5.5)
testvector = V.generate 4 (\i -> 23.2)

-- | get dimensions of matrix as string
dim :: M.Matrix a -> String
dim m = (show $ M.nrows m) ++ ", " ++ (show $ M.ncols m)

index3 x f = nitems + nfeatures * (x-1) + f

index2 :: Int 
       -> Int
       -> Int
index2 x f = (x-1) * nfeatures + f

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

bstr2vec :: Bl.ByteString -> V.Vector (Int,Int,Double)
bstr2vec bstr = toVec (decode NoHeader bstr :: Either String (V.Vector (Int,Int,Double)))

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


cost :: M.Matrix Double -> Parameter -> Double
cost y p =  sumOfErrors $ errors y p
 where sumOfErrors es = sum [r^2|(r,_) <- (filter (\(x,y) -> y /= 0.0) es) ]

cost2 :: M.Matrix Double -> (Features, Features) -> Double
cost2 y (x,theta) = sum $ map (\(i,u) -> eui2 (rui (i,u) y) (M.getElem i 1 x) (M.getElem u 1 theta)) $ trainingcases y

errors :: M.Matrix Double -> Parameter -> [(Double, Double)]
errors y p = zipWith (\r p1 -> (r - p1, r)) (M.toList y) (M.toList $ multiplym p)

multiplym :: Parameter -> M.Matrix Double
multiplym (pi, pu) = M.multStd pi (M.transpose pu)

module Main where

import Data.Csv
import Userbased as Ub
import Bias as Bi
import System.Environment
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as Bl
import qualified Data.MultiMap as MM
import qualified Data.Map as M
import qualified Matrixfactorization as Mf

type Rating = (Int, Int, Double)
type User = Int
type Item = Int

main = do
  (command:argList) <- getArgs
  dispatch command argList

dispatch :: String -> [String] -> IO ()
dispatch "userbased" = userbased
dispatch "bias" = bias 
dispatch "mf" = matrixfactorization
dispathc _ = bias

matrixfactorization :: [String] -> IO ()
matrixfactorization (base:test:xs) =  do
  putStrLn "Start userbased recommender"
  (traindata, testdata) <- loadData base test
  m <- Mf.model traindata
  let bm = Bi.model traindata
  putStrLn $ "Mean Absolute Error: " ++ (show $ mae (Mf.predict m bm) testdata)  

userbased :: [String] -> IO ()
userbased (base:test:xs) = do
  putStrLn "Start userbased recommender"
  putStrLn "Loading Data"
  (modelv, testv) <- loadData base test
  let m = Ub.model modelv 
  putStrLn $ "Mean Absolute Error: " ++ (show $ mae (Ub.predict m) testv)  

toVec :: Either String (V.Vector Rating)
      -> V.Vector Rating
toVec (Left err) = error err
toVec (Right v) = v

mae :: (Int -> Int -> Double) -> V.Vector Rating -> Double
mae p v = (V.sum $ errors p v) / (fromIntegral (V.length v))

errors :: ( Int -> Int -> Double )
       -> V.Vector Rating
       -> V.Vector Double
errors p v = V.filter (\x -> not $ isNaN x) $ V.map (\(u, i, r) -> abs (r - p u i)) v
  where diff (u, i, r) = abs (r - p u i)

bias (base:test:xs) = do
  putStrLn "Start bias recommender"
  putStrLn "Loading Data"
  (modelv, testv) <- loadData base test
  let (mu,urm,ium,uirm) = Bi.model modelv 
  putStrLn $ "Mean Absolute Error: " ++ (show $ mae (Bi.predict mu urm ium uirm) testv)  

loadData :: String -> 
            String ->
            IO (V.Vector Rating, V.Vector Rating) 
loadData basefile testfile = do
  c1 <- Bl.readFile basefile
  c2 <- Bl.readFile testfile
  let baseData = decode NoHeader c1 :: Either String (V.Vector Rating)
  let testData = decode NoHeader c2 :: Either String (V.Vector Rating)
  let modelv =  toVec baseData
  let testv = toVec testData
  return (modelv, testv)

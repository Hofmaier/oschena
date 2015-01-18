module Main where

import Data.Csv
import Userbased
import System.Environment
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as Bl
import qualified Data.MultiMap as MM
import qualified Data.Map as M

type Rating = (Int, Int, Double)
type User = Int
type Item = Int

basefile = "/home/lukas/oschena/ml-100k/base.csv"
testfile = "/home/lukas/oschena/ml-100k/test.csv"
main = do
  (command:argList) <- getArgs
  dispatch command argList

userbased :: [String] -> IO ()
userbased _ = do
  putStrLn "Loading Data"
  c1 <- Bl.readFile basefile
  c2 <- Bl.readFile testfile
  let baseData = decode NoHeader c1 :: Either String (V.Vector Rating)
  let testData = decode NoHeader c2 :: Either String (V.Vector Rating)
  let modelv =  toVec baseData
  let testv = toVec testData
  let m = model modelv 
  putStrLn $ "Mean Absolute Error: " ++ (show $ userbasedmae m testv)

toVec :: Either String (V.Vector Rating)
      -> V.Vector Rating
toVec (Left err) = error err
toVec (Right v) = v

userbasedmae :: Model -> V.Vector Rating ->  Double
userbasedmae m v = V.sum errors / (fromIntegral (V.length v))
                   where errors = V.filter (\x -> not $ isNaN x) $ V.map (\(u, i, r) -> abs (r - (p1 u i m))) v

mae :: (Int -> Int -> Double) -> V.Vector Rating -> Double
mae p v = (V.sum $ errors p v) / (fromIntegral (V.length v))

errors :: ( Int -> Int -> Double )
       -> V.Vector Rating
       -> V.Vector Double
errors p v = V.filter (\x -> not $ isNaN x) $ V.map (\(u, i, r) -> abs (r - p u i)) v

predictbu :: Double 
          -> MM.MultiMap Int Double
          -> MM.MultiMap Item User
          -> M.Map User (M.Map Item Double)
          -> Int 
          -> Int 
          -> Double
predictbu mu urm ium uirm us it = mu + (bu mu urm us) + bi
                       where dev = map (\x -> rating x it - (bu mu urm x) - mu) users
                             s = sum $ dev 
                             n = fromIntegral $ length users
                             rating u i = rui u i (irm u)  
                             userlength = fromIntegral $ length users
                             bi = s / n
                             rui u i m = M.findWithDefault 0 i m
                             users = MM.lookup it ium
                             irm u = M.findWithDefault M.empty u uirm

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

dispatch :: String -> [String] -> IO ()
dispatch "userbased" = userbased
dispatch _ = bias 

bias _ = do
  putStrLn "Loading Data"
  c1 <- Bl.readFile basefile
  c2 <- Bl.readFile testfile
  let baseData = decode NoHeader c1 :: Either String (V.Vector Rating)
  let testData = decode NoHeader c2 :: Either String (V.Vector Rating)
  let modelv =  toVec baseData
  let testv = toVec testData
  let (mu,urm,ium,uirm) = model2 modelv 
  putStrLn $ "Mean Absolute Error: " ++ (show $ mae (predictbu mu urm ium uirm) testv)  

module Main where

import Data.Csv
import Userbased
import System.Environment
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as Bl

type Rating = (Int, Int, Double)

basefile = "/home/lukas/oschena/ml-100k/base.csv"
testfile = "/home/lukas/oschena/ml-100k/1000.csv"
main = do
  (command:argList) <- getArgs
  dispatch command argList

userbased :: [String] -> IO ()
userbased [basefile,testfile] = do
  putStrLn "Loading Data"
  c1 <- Bl.readFile basefile
  c2 <- Bl.readFile testfile
  let baseData = decode NoHeader c1 :: Either String (V.Vector Rating)
  let testData = decode NoHeader c2 :: Either String (V.Vector Rating)
  let modelv =  toVec baseData
  let testv = toVec testData
  let m = model modelv 
  putStrLn $ "Mean Absolute Error: " ++ (show $ userbasedmae m testv)
userbased _ = error "too few arguments"

toVec :: Either String (V.Vector Rating)
      -> V.Vector Rating
toVec (Left err) = error err
toVec (Right v) = v

userbasedmae :: Model -> V.Vector Rating ->  Double
userbasedmae m v = V.sum errors / (fromIntegral (V.length v))
                   where errors = V.filter (\x -> not $ isNaN x) $ V.map (\(u, i, r) -> abs (r - (p1 u i m))) v

dispatch :: String :: -> [String] -> IO()
dispatch "userbased" = userbased

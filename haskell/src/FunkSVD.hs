module FunkSVD where

import Data.Matrix
import qualified Data.Vector as V

type Parameters = (Matrix Float, Matrix Float)

ymat = fromList rnr_item rnr_user [5, 4, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0]
rnr_user = 4
rnr_item = 5
rn_features = 3
alpha = 0.001
lambda = 10
nr_iter = 1
initialitemlf = [1.048686, -0.400232, 1.194119, 0.780851, -0.385626, 0.521198, 0.641509, -0.547854, -0.083796, 0.453618, -0.800218, 0.680481, 0.937538, 0.106090, 0.361953]

itemlf = fromList rnr_item rn_features initialitemlf

userlatentfactors = [0.28544, -1.68427, 0.26294, 0.50501, -0.45465, 0.31746, -0.43192, -0.47880, 0.84671, 0.72860, -0.27189, 0.32684]

userlf = fromList rnr_user rn_features userlatentfactors
parameters = (itemlf, userlf)
cost y p = sum [r^2|(r,_) <- (filter f3 $ errlist2 y p)]
f3 (res, rating) = rating /= 0
reg (pu, qi) = lambda * (sqm pu + (sqm pu))
                      where sqm m = (mag $ lengthofrows m)^2

lengthofrows :: Matrix Float -> [Float]
lengthofrows m = [mag x|x <- toLists m]

mag :: [Float] -> Float
mag xs = sqrt $ foldl (\acc x -> acc + (x^2)) 0 xs

magv = mag [1,2,0,-1]

y2r :: Matrix Float -> Matrix Float
y2r y = fromList (nrows y) (ncols y) [if x == 0 then 0 else 1 | x <- toList y]

y2rv :: Matrix Float
y2rv = y2r ymat

errc y p = sum $ errlist y p

errlist y p = zipWith serr (toList y) (pwmult p (y2r 2)) 
errlist2 :: Matrix Float -> (Matrix Float, Matrix Float) -> [(Float, Float)]
errlist2 y p = zipWith res (toList y) (toList $ multiplypq p)
res r p = (r - p, r)
multiplypq (pi, pu) = multStd pi (transpose pu)

pwmult (pu, qi) r = zipWith (\x y -> x * y) (toList $  qi * (transpose pu)) (toList r)

serr actual prediction = (actual - prediction)^2

costv = cost ymat (userlf, itemlf)

traini:: (Matrix Float, Matrix Float) -> (Int,Int) -> Float -> Matrix Float
traini (qi, pu) (i,u) error = mapRow (upRow (uservector u pu) u error) i qi

uservector u pu = getRow u pu

upRow :: V.Vector Float -> Int -> Float -> Int -> Float -> Float
upRow v u e col x = x +(alpha * (e * (v V.! (col-1))))

generateindex y = [(x,y) | x <- [1..nrows y], y <- [1..ncols y]]

trainingset :: Matrix Float -> (Matrix Float, Matrix Float) -> [(Float, (Int, Int))]
trainingset y p =  [(res, i) | ((res, rating), i) <- (filter f2 (zip (errlist2 y p) (generateindex y)))]

f2 ((res, rating), i) = rating /= 0


--loop throug  trainingsets
g :: Matrix Float -> (Matrix Float, Matrix Float) -> Int -> (Matrix Float, Matrix Float)
g y p _  = foldl f p (trainingset y p)

f :: (Matrix Float, Matrix Float) -> (Float, (Int, Int)) ->  (Matrix Float, Matrix Float )
f (qi, pu) (error, (i,u)) = (updaterow (qi, pu) (i,u) error, updaterow (pu, qi) (u,i) error)

iterateg y p = foldl (g y) p [1..nr_iter]

updaterow :: Parameters -> (Int,Int) -> Float -> Matrix Float
updaterow (qi,pu) i e = foldl (u2 (qi,pu) i e) qi [1 .. (ncols qi)]

u2 :: Parameters -> (Int,Int) -> Float -> Matrix Float -> Int -> Matrix Float
u2 (x,theta) (i,u) error acc col = setElem (computelf itemvalue uservalue error) (i,col) acc
  where itemvalue = getElem i col x
        uservalue = getElem u col theta

computelf :: Float -> Float -> Float -> Float
computelf itemvalue uservalue err = itemvalue + (alpha * (err * uservalue))

train2 :: Parameters -> (Int,Int) -> Float -> Matrix Float
train2 p i error = updaterow p i error

predict :: Int -> Int -> Parameters -> Float
predict u i (x, theta) = V.sum (V.map (\(a,b) -> a*b) (ziu i u (x,theta)))

ziu :: Int -> Int -> Parameters -> V.Vector (Float, Float)
ziu i u (x,theta) = V.zip (piv i x) (pu u theta)

piv :: Int -> Matrix Float -> V.Vector Float
piv i m = getRow i m
 
pu :: Int -> Matrix Float -> V.Vector Float
pu u m = getRow u m


module Evaluation

where

import MovieLensData as Ml

main = do
  putStrLn "start evaluation"
  r <- Ml.loadUirs Ml.filepath
  t <- Ml.loadUirs Ml.testsetpath
  putStrLn ("bu mae: " ++ (show $ maebu r t))


maebu :: [UserItemRating] -> [UserItemRating] -> Float
maebu r t = (sum $ errors $ prlist r t) / (fromIntegral $ length t)

errors :: [(Int, Float)] -> [Float]
errors pqs = [abs $ (fromIntegral p) -  q | (p,q) <- pqs]

prlist :: [UserItemRating] -> [UserItemRating] -> [(Int, Float)]
prlist rs t = zip [ r | (u:i:r:_) <- t] $ bulist rs t

bulist:: [UserItemRating] -> [UserItemRating] -> [Float]
bulist r t = [singlebu r x |x <- t]

singlebu :: [UserItemRating] -> UserItemRating -> Float
singlebu r (u:i:_) = Ml.bu u i r


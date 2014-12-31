predict :: User
   -> Item
   -> Model
   -> Int
   -> Double
predict user item model k = rating / normalization
  where neighborhood = knn user item model k
        normalization = sum [ s | (s,_,_) <- neighborhood]
        rating = sum [s * r | (s, r, u) <- neighborhood]
                                                               
shareditems :: Int -> Int -> Multimap User Item -> [Int]
shareditems u1 u2 m = shared (lookup u1 m) (lookup u2 m)
  where shared l1 l2 = [x| x <- l1, y <- l2, x == y]


shared :: [Int] -> [Int] -> [Int]
shared l1 l2 = [x| x <- l1, y <- l2, x == y]

import Data.MultiMap
item2rating:: [Item] -> Multimap Item Double -> [Double]
item2rating is m = map $ (\i -> findWithDefault 0 i m) is

neihborhood :: User
            -> Item
            -> Int
            -> MultiMap User [(Similarity, User)]
            -> [User]
neihborhood u i k m = take k (reverse (sort onlyItem i u ))
  where onlyItem i = filter (hasRated i) allneighbors
        allneighbors = lookup u m

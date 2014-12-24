predict :: User
   -> Item
   -> Model
   -> Int
   -> Double
predict user item model k = rating / normalization
  where neighborhood = knn user item model k
        normalization = sum [ s | (s,_,_) <- neighborhood]
        rating = sum [s * r | (s, r, u) <- neighborhood]

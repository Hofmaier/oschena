similarity :: User -> User -> Maybe Double
similarity u v = Statistics.pearson r1 r2
                 where (r1, r2) = ratings

ratings::User->User->([Double],[Double])
rarings u1 u2=(lookupR u1 shared, lookupR u2 shared)
  where shared = shareditems u1 u2

lookupR::User->[Item]->Maybe[Double]
lookupR u is = do
  ratingmap <- lookup u useritemMap
  foldl (\acc x -> (lookup x ratingmap):acc) [] is

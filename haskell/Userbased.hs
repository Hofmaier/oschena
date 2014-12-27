module Userbased where

import qualified Data.Map as Map
import Math.Statistics as Statistics

type User = Int
type Item = Int

similarity :: User -> User -> Maybe Double
similarity u v = Statistics.pearson r1 r2
                 where r1 = ratings u si
                       r1 = ratings v si
                       si = shareditems u v

ratingsOfsharedItems :: User -> User -> ([Double], [Double])
raringsOfsharedItem u1 u2 = (lookupRatings u1 shared, lookupRatings u2 shared)
                            where shared = shareditems u1 u2

lookupRating :: User -> [Item] -> Maybe [Double]
lookupRating u is = do
  itemratingmap <- lookup u useritemMap
  foldl (\acc x -> (lookup x itemratingmap):acc) [] i

shareditems :: User -> User -> [Item]
shareditems u1 u2==[x|i1<-is1,i2<-is2,x == y]
  where is1=lookup u1 itemMap
        is2=lookup u2 itemMap





module Matrixfactorization
(gradientDescent
)
where

import Data.Vector

cost :: Matrix Int -> Matrix Int -> Vector Int -> Vector Int

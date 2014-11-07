module Matrixfactorization
(
)
where

import Data.Vector as V
import MovieLensData as Ml
import Data.Matrix as M

type Gradient = Matrix Float
type Parameter = (Matrix Float, Matrix Float)
type Ratings = Matrix Float
type CostFunc = Parameter -> (Gradient, Float)

iter = 100

alpha = 0.01

rnr_user = 4
rnr_item = 5
rn_features = 3

lambda :: Float
lambda = 10

rratings :: [Float]
rratings = [1.048686, -0.400232, 1.194119, 0.780851, -0.385626, 0.521198, 0.641509, -0.547854, -0.083796, 0.453618, -0.800218, 0.680481, 0.937538, 0.106090, 0.361953]

itemlfmat :: Matrix Float
itemlfmat = M.fromList rnr_item rn_features rratings

userlatentfactors :: [Float]
userlatentfactors = [0.28544, -1.68427, 0.26294, 0.50501, -0.45465, 0.31746, -0.43192, -0.47880, 0.84671, 0.72860, -0.27189, 0.32684]

userlfmat :: M.Matrix Float
userlfmat = M.fromList rnr_user rn_features userlatentfactors

ymat :: Matrix Float
ymat = M.fromList rnr_item rnr_user [5, 4, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0]

j :: Float
j =  costvalue (y2r ymat) (sqerr (err itemlfmat userlfmat ymat)) (reg itemlfmat userlfmat)

o :: Matrix Float
o = piecewisem (y2r ymat) (sqerr (err itemlfmat userlfmat ymat))

m :: Matrix Float
m = sqerr $ errv

errv :: Matrix Float
errv = err itemlfmat userlfmat ymat

r :: Matrix Float
r = y2r ymat

y2r :: Matrix Float -> Matrix Float
y2r y = Prelude.foldl (\acc x -> M.mapRow (\_ x -> if x == 0 then 0 else 1) x acc) y [1 .. M.nrows y]

sumsq :: Matrix Float -> [Float]
sumsq m = [V.sum ( msquare (M.getCol x m)) | x <- [1 .. M.ncols m]]

summ :: Matrix Float -> Float
summ m = Prelude.sum [V.sum (M.getRow x m) | x <-[1 .. M.nrows m]]

summv :: Float
summv = summ piecewisemv

reg :: Matrix Float -> Matrix Float -> Float
reg x theta =  (lambda / 2) * ((Prelude.sum $ sumsq x) + (Prelude.sum $ sumsq theta))

err :: Matrix Float -> Matrix Float -> Matrix Float -> Matrix Float
err x theta y = (M.multStd x $ M.transpose theta) - y

sqerr :: Matrix Float -> Matrix Float
sqerr m = Prelude.foldl (\acc x -> M.mapRow (\_ e -> e^2) x acc) m [1 .. M.nrows m]

piecewisem :: Matrix Float -> Matrix Float -> Matrix Float
piecewisem v u = matrix (M.nrows v) (M.ncols u) (\(i, j) -> (M.getElem i j v) * (M.getElem i j u))

costvalue :: Matrix Float -> Matrix Float -> Float -> Float
costvalue r m reg =  ((summ (piecewisem r m)) / 2.0) + reg

gradientdescent :: Parameter -> Ratings -> Parameter
gradientdescent p y = Prelude.foldl (gradientstep y)  p [1 .. 100]

gradientstep :: Ratings -> Parameter -> Int -> Parameter
gradientstep y (xp, thetap) i = (subgrad xp thetap y, subgrad thetap xp y)

subgrad :: Matrix Float -> Matrix Float -> Ratings -> Matrix Float
subgrad x theta y = x - (M.scaleMatrix alpha (xgrad (x,theta) y))

costfunc :: Parameter -> Ratings -> (Gradient, Float)
costfunc p y = (y, 100.0)

cost :: (Matrix Float -> Matrix Float -> Matrix Float -> (Matrix Float, Matrix Float, Float))
cost x theta y = (x, x, costvalue (y2r y) (sqerr (err x theta y)) (reg x theta))

xgrad :: Parameter -> Ratings -> Gradient
xgrad (x, theta) y = (M.multStd (piecewisem e r) theta) + M.scaleMatrix lambda x
                  where r = y2r y
                        e = err x theta y



sqerrv :: Matrix Float
sqerrv = sqerr errv

piecewisev :: Vector Float -> Vector Float -> Vector Float
piecewisev v u = V.generate (V.length v) (\x -> (v V.! x) * (u V.! x))

piecewisemv :: Matrix Float
piecewisemv = piecewisem sqerrv $ y2r ymat

regv :: Float
regv = reg itemlfmat userlfmat

sqerrm :: Matrix Float
sqerrm = M.fromList 2 2 [1,2,3,4]

msquare :: Vector Float -> Vector Float
msquare xs = V.map (\x -> x^2) xs

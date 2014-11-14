module Matrixfactorization
(
cost,
cost2,
costtest,
gradientdescent,
ratings,
latentfactors,
y2r
)
where

import Data.Vector as V
import MovieLensData as Ml
import Data.Matrix as M

type Gradient = Matrix Float
type Parameter = (Matrix Float, Matrix Float)
type Ratings = Matrix Float
type CostFunc = Parameter -> (Gradient, Float)

iter = 10

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

initialparameter = (itemlfmat, userlfmat)

ymat :: Matrix Float
ymat = M.fromList rnr_item rnr_user [5, 4, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0]

j :: Float
j =  costvalue (y2r ymat) (sqerr (err itemlfmat userlfmat ymat)) (reg itemlfmat)

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

reg :: Matrix Float -> Float
reg m =  Prelude.sum $ sumsq m

err :: Matrix Float -> Matrix Float -> Matrix Float -> Matrix Float
err x theta y = (M.multStd x $ M.transpose theta) - y

sqerr :: Matrix Float -> Matrix Float
sqerr m = Prelude.foldl (\acc x -> M.mapRow (\_ e -> e^2) x acc) m [1 .. M.nrows m]

piecewisem :: Matrix Float -> Matrix Float -> Matrix Float
piecewisem v u = matrix (M.nrows v) (M.ncols u) (\(i, j) -> (M.getElem i j v) * (M.getElem i j u))

costvalue :: Matrix Float -> Matrix Float -> Float -> Float
costvalue r m reg =  ((summ (piecewisem r m)) / 2.0) + reg

gradientdescent :: Parameter -> Ratings -> Matrix Float -> Parameter
gradientdescent p y r = Prelude.foldl (gradientstep y r)  p [1 .. iter]

gradientstep :: Ratings -> Matrix Float -> Parameter -> Int -> Parameter
gradientstep y r (xp, thetap) i = (subgrad xp thetap y r, subgrady (xp, thetap) y r)

subgrad :: Matrix Float -> Matrix Float -> Ratings -> Matrix Float -> Matrix Float
subgrad x theta y r = x - (M.scaleMatrix alpha (xgrad (x,theta) y r))

subgrady :: Parameter -> Ratings -> Matrix Float -> Gradient 
subgrady (x, theta) y r = theta - (M.scaleMatrix alpha (ygrad (x, theta) y r))

costfunc :: Parameter -> Ratings -> (Gradient, Float)
costfunc p y = (y, 100.0)

cost :: Parameter -> Ratings -> Float
cost (x, theta) y = costvalue (y2r y) (sqerr (err x theta y)) (reg x)

cost2 :: Parameter -> Ratings -> Float
cost2 (x, theta) y =  (error2/2 ) + regularization
  where error2 = Prelude.sum $ [ rdiff p r | (p, r) <- knownratings (x, theta) y]
        regularization = (lambda / 2) * ((reg x) + (reg theta))

error3 :: Parameter -> Ratings -> Float
error3 (x,theta) y = Prelude.sum $ [ rdiff p r | (p, r) <- knownratings (x, theta) y]

error3v = error3 latentfactors ymat

rdiff :: Float -> Float -> Float
rdiff p r = (r - p)^2

knownratings :: Parameter -> Ratings -> [(Float, Float)]
knownratings p r = Prelude.filter (\(p, r) -> r /= 0.0) $ yrzip p r

yrzip :: Parameter -> Ratings -> [(Float, Float)]
yrzip (x, theta) y = Prelude.zip (M.toList (M.multStd x $ M.transpose theta)) $ M.toList y

xgrad :: Parameter -> Ratings -> Gradient -> Matrix Float
xgrad (x, theta) y r = (M.multStd (piecewisem e r) theta) + M.scaleMatrix lambda x
  where e = err x theta y

ygrad :: Parameter -> Ratings -> Matrix Float -> Gradient 
ygrad (x, theta) y r = (M.multStd (M.transpose (piecewisem e r)) x) + M.scaleMatrix lambda theta
  where e = err x theta y

main = do
  y <- Ml.loadMatrix Ml.yrpath
  x <- Ml.loadMatrix Ml.xrpath
  theta <- Ml.loadMatrix Ml.thetarpath
  r <- Ml.loadMatrix Ml.rpath
  putStrLn $ "run gradient descent: " Prelude.++ (show $ gradientdescent (x,theta) y r)
            
sqerrv :: Matrix Float
sqerrv = sqerr errv

piecewisev :: Vector Float -> Vector Float -> Vector Float
piecewisev v u = V.generate (V.length v) (\x -> (v V.! x) * (u V.! x))

piecewisemv :: Matrix Float
piecewisemv = piecewisem sqerrv $ y2r ymat

regv :: Float
regv = reg itemlfmat

reg2v :: Float
reg2v = (lambda / 2) * ((reg itemlf) + (reg userlf))

sqerrm :: Matrix Float
sqerrm = M.fromList 2 2 [1,2,3,4]

msquare :: Vector Float -> Vector Float
msquare xs = V.map (\x -> x^2) xs

ratings = ymat
itemlf = itemlfmat
userlf = userlfmat
latentfactors = (itemlf, userlf)
errorfunc = cost
costtest = cost latentfactors ratings


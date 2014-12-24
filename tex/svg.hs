train :: Features
      -> (Int,Int) 
      -> M.Matrix Double 
      -> Features
train (x, theta) (i, u) y
  = (updateRow i newqi x, updateRow u newpu theta)
  where newqi = f4 oldqi oldpu error
        newpu = f4 oldpu oldqi error
        oldqi = M.getElem i 1 x
        oldpu = M.getElem u 1 theta
        error = eui2 (rui (i,u) y) oldqi oldpu

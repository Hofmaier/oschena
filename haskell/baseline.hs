bui :: User -> Item -> Double
bui u i = mu + bu + bi
  where mu = avg allratings
        bu = computebu u
        bi = computebu i

computebu :: User -> Double
computebu u = sum lookup u usermap

computebi :: Item -> Double
computebi i = sum lookup u itemmap

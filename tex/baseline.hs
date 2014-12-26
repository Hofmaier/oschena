bui :: User -> Item -> Double
bui u i = mu + bu + bi
  where mu = avg allratings
        bu = computebu u
        bi = computebu i

computebu :: User -> Double
computebu u = sum ratings / nrOfratings
              where ratings = lookup u usermap
                    nrOfratings = length ratings

computebi :: Item -> Double
computebi i = sum lookup u itemmap

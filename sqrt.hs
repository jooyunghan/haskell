
sqrt' :: Float -> Float
sqrt' x = until goodenough improve x
           where goodenough y = abs (y * y - x) < eps * x
                 improve y = (y + x/y) / 2
                 eps = 0.000001

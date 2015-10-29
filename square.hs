
sl :: Num a => [a] -> [a]
sl [] = []
sl (x:xs) = (x*x) : sl xs
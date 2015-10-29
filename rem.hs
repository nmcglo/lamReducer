rem' :: Eq t => [t] -> t -> [t]
rem' xs i = [ x | x <- xs, not (i == x)]
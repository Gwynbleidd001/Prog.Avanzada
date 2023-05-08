retDigts :: (Num a) => [a] -> a
retDigts [x] = x
retDigts (x:xs) = hd(x:xs)
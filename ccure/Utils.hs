module Utils where


extractFirst4 :: (a, b, c, d) -> a
extractFirst4 (a, _, _, _) = a

extractThird3 :: (a, b, c) -> c
extractThird3 (_, _, c) = c

extractFourth4 :: (a, b, c, d) -> d
extractFourth4 (_, _, _, d) = d
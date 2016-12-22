count :: (Integral a) => a -> a -> a
count 0 _ = 0
count _ 0 = 0
count 1 1 = 1
count 1 h = 1 + count 1 (h - 1)
count w 1 = 1 + count (w - 1) 1
count w h = 1 +
            count 1 h +
            count (w - 1) h +
            count w 1 +
            count w (h - 1)

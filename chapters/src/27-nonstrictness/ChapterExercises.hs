{-# LANGUAGE BangPatterns #-}

x = undefined
y = "blah"

withSeq = print (snd (x, x `seq` y))
withBang = let !x' = x in snd (x', y)

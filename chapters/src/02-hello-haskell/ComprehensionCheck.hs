module ComprehensionCheck where

piSquared' :: Fractional x => x -> x
piSquared' x = 3.14 * (x * x)

piSquared :: Floating x => x -> x
piSquared x = pi * (x * x)

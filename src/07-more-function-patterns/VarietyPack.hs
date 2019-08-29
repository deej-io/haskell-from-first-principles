module VarietyPack where

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (x, y, z) (x', y', z') = ((x, x'), (z, z'))

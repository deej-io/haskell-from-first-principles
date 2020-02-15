module Main where

import Criterion.Main

newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL $ const []
{-# INLINE empty #-}

singleton :: a -> DList a
singleton a = DL $ const [a]
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList dl = unDL dl []
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x dl = DL $ \xs -> x : unDL dl xs
{-# INLINE cons #-}

infixr `snoc`
snoc :: a -> DList a -> DList a
snoc x dl = DL $ \xs -> unDL dl xs ++ [x]
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append lhs rhs = DL $ \xs -> unDL lhs xs ++ unDL rhs xs
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs =
          go (n-1)
          (singleton n `append` xs)

main :: IO ()
main = do
  print $ schlemiel 123456 == constructDlist 123456
  defaultMain
    [ bench "concat list" $
      whnf schlemiel 123456
    , bench "concat dlist" $
      whnf constructDlist 123456
    ]



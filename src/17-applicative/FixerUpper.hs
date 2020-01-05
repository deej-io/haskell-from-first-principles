module FixerUpper where

x = const <$> Just "Hello" <*> pure "World"

y = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

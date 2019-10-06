module ChapterExercises where

import           Data.Char

notThe :: String -> Maybe String
notThe str | fmap toLower str == "the" = Nothing
           | otherwise                 = Just str

replaceThe :: String -> String
replaceThe = unwords . map replaceThe' . words
        where replaceThe' word = fromMaybe "a" (notThe word)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = foldr countTheBeforeVowel' 0 (zip ws (drop 1 ws))
    where
        ws = words str
        countTheBeforeVowel' (w1, c : w2) acc =
                if isNothing (notThe w1) && isVowel c then acc + 1 else acc


isVowel :: Char -> Bool
isVowel = flip elem vowels

countVowels :: String -> Int
countVowels = length . filter isVowel

newtype Word' = Word' String
                deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord word | vowels < consonants = Just $ Word' word
            | otherwise           = Nothing
        where (vowels, consonants) = partition isVowel word

partition :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
partition f = foldr partition' ([], [])
    where
        partition' c (vs, cs) | f c       = (vs ++ [c], cs)
                              | otherwise = (vs, cs ++ [c])

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = natToInteger x + 1

integerToNat :: Integer -> Maybe Nat
integerToNat n | n < 0     = Nothing
               | otherwise = Just $ fromInteger' n
    where
        fromInteger' 0 = Zero
        fromInteger' n = Succ . fromInteger' $ n - 1

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe _ f (Just a) = f a
mayybe d _ _        = d

fromMaybe :: a -> Maybe a -> a
fromMaybe a = maybe a id

listToMaybe :: [a] -> Maybe a
listToMaybe (x : xs) = Just x
listToMaybe _        = Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList _        = []

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr catM [] where catM ma as = maybeToList ma ++ as

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe []            = Just []
flipMaybe (Just x : xs) = fmap (x :) (flipMaybe xs)
flipMaybe _             = Nothing

lefts :: [Either a b] -> [a]
lefts []            = []
lefts (Left a : as) = a : lefts as
lefts (_      : as) = lefts as

rights :: [Either a b] -> [b]
rights []             = []
rights (Right a : as) = a : rights as
rights (_       : as) = rights as

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr partitionEithers' ([], [])
    where
        partitionEithers' (Left  l) (ls, rs) = (ls ++ [l], rs)
        partitionEithers' (Right r) (ls, rs) = (ls, rs ++ [r])

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe f (Right r) = Just . f $ r
eitherMaybe _ _         = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left  l) = f l
either' _ g (Right r) = g r

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f = either (const Nothing) (Just . f)

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f x

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f b = case f b of
        Just (a, b) -> a : unfoldr' f b
        _           -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = unfoldr' (\x -> Just (x, f x))

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing -> Leaf
  Just (a1, b, a2) -> Node (unfold f a1) b (unfold f a2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild i =  unfold loop 0
  where loop n
         | n == i = Nothing
         | otherwise = Just (n + 1, n, n + 1)

module Typecheck where

data Person = Person Bool
    deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah | Woot
    deriving Eq

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                then Blah
                else x

type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"

s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"

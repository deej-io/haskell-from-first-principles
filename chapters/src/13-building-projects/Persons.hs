module Persons where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0
  = Right $ Person name age
  | name == ""
  = Left NameEmpty
  | age <= 0
  = Left AgeTooLow
  | otherwise
  = Left
    $  PersonInvalidUnknown
    $  "Name was: "
    ++ name
    ++ "Age was: "
    ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Name: "
  name <- getLine
  putStr "Age: "
  age <- getLine
  putStrLn $ case mkPerson name (read age) of
    Right person -> "Yay! Successfully got a person: " ++ show person
    Left  err    -> "ERROR: " ++ show err

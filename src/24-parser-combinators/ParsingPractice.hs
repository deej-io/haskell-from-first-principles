module Ch24.ParsingPractice where

import Text.Trifecta
import Control.Applicative

stop :: Parser ()
stop = unexpected "stop"

one = char '1'
one' = one >> stop
oneEof = one >> eof

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop
oneTwoEof = oneTwo >> eof

p123 :: Parser String
p123 = string "123" <|> string "12" <|> string "1"

testParse :: Show a => Parser a -> IO ()
testParse p = print $ parseString p mempty "123"

string' :: String -> Parser String
string' str = parse str mempty
  where
    parse (c:cs) parsed = char c >>= (\c' -> parse cs (parsed ++ [c']))
    parse [] parsed     = return parsed

parseInt :: Parser Integer
parseInt = integer <* eof

pNL s =
  putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneEol:"
  testParse oneEof
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneTwoEof:"
  testParse oneTwoEof
  pNL "p123:"
  testParse p123
  pNL "parseInt:"
  testParse parseInt

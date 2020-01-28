module Ch24.SemVer where

import Control.Applicative
import Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer
  deriving (Eq, Ord, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
newtype Release = Release [NumberOrString] deriving (Eq, Show)
newtype Metadata = Metadata [String] deriving (Eq, Show)

data SemVer = SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

positiveDigit :: Parser Char
positiveDigit = do
  digit <- digit
  case digit of
    '0' -> fail "digit should be non-zero"
    _   -> return digit

nonDigit :: Parser Char
nonDigit = letter <|> char '-'

idChar :: Parser Char
idChar = alphaNum <|> char '-'

-- makes it easier to combine Char and String parsers
asMany :: Parser a -> Parser [a]
asMany = fmap (:[])

-- <numeric identifier> ::= "0"
--                        | <positive digit>
--                        | <positive digit> <digits>
parseNumericId :: Parser Integer
parseNumericId = read <$> (string "0" <|> asMany positiveDigit <> many digit)

-- <alphanumeric identifier> ::= <non-digit>
--                             | <non-digit> <identifier characters>
--                             | <identifier characters> <non-digit>
--                             | <identifier characters> <non-digit> <identifier characters>
parseAlphaNumericId :: Parser NumberOrString
parseAlphaNumericId = NOSS <$> (
  asMany nonDigit <> many idChar <|>
  some idChar <> asMany nonDigit <> many idChar)

orEmpty :: Parser [a] -> Parser [a]
orEmpty p = try p <|> return []

parseRelease :: Parser Release
parseRelease = Release <$> orEmpty (char '-' *> sepBy1 (NOSI <$> parseNumericId <|> parseAlphaNumericId) (char '.'))

parseMetadata :: Parser Metadata
parseMetadata = Metadata <$> orEmpty (char '+' *> sepBy1 (many idChar) (char '.'))

parseSemVer :: Parser SemVer
parseSemVer = SemVer <$>
  parseNumericId <* char '.' <*>
  parseNumericId <* char '.' <*>
  parseNumericId <*>
  parseRelease <*>
  parseMetadata <* eof

instance Ord Release where
  (Release []) <= (Release []) = True
  (Release []) <= (Release (_:_)) = False
  (Release xs) <= (Release ys) = xs <= ys

instance Ord SemVer where
  (SemVer major1 minor1 patch1 release1 _) <= (SemVer major2 minor2 patch2 release2 _) =
    and [major1 <= major2, minor1 <= minor2, patch1 <= patch2, release1 <= release2]
  
-- tests

tests :: IO ()
tests = do
  print $ parseString parseAlphaNumericId mempty "beta"
  print $ parseString parseSemVer mempty "2.1.1"
  print $ parseString parseSemVer mempty "1.0.3-beta.foo-bar.23+exp.sha.5114f85"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  print $ parseString parseSemVer mempty "1.0.0-beta+oof.sha.41af286"


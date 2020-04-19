import Control.Applicative
import Data.Char

main = interact (show . fmap fst . runParser jsonParser)

data Json
 = JNull
 | JBool Bool
 | JNumber Int
 | JString String
 | JArray [Json]
 | JObject [(String, Json)]
 deriving (Show, Eq)

newtype Parser a = Parser {
  runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
  fmap f (Parser p) = Parser q
    where q input = fmap g (p input)
          g (y, rest) = (f y, rest)

instance Applicative Parser where
  pure x = Parser (\input -> Just (x, input))
  (Parser p) <*> (Parser q) = Parser r
    where r input = (p input) >>= runSecond
          runSecond (y, rest) = (q rest) >>= \(y', rest') -> Just (y y', rest')

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  (Parser p) <|> (Parser q) = Parser (\input -> p input <|> q input)

notNull :: Parser [a] -> Parser [a]
notNull (Parser f) = Parser p
  where p input = (f input) >>= fail
        fail (ys, rest) = if null ys then Nothing else Just (ys, rest)

charParser :: Char -> Parser Char
charParser c = Parser f
  where f "" = Nothing
        f (c':cs)
          | c == c' = Just (c, cs)
          | otherwise = Nothing

stringParser :: String -> Parser String
stringParser = sequenceA . map charParser

spanParser :: (Char -> Bool) -> Parser String
spanParser f = Parser (\input -> Just (span f input))

separatedBy :: Parser String -> Parser a -> Parser [a]
separatedBy sep item = (:) <$> item <*> many (sep *> item) <|> pure []

wsParser :: Parser String
wsParser = many (charParser ' ')

stringLitParser :: Parser String
stringLitParser = charParser '"' *> spanParser (/= '"') <* charParser '"'

-- Caveats:
-- Cannot parse strings with escaped characters
-- Can only parse numbers in integer form
jsonParser :: Parser Json
jsonParser = wsParser *>
  (jNullParser <|> jBoolParser <|> jNumberParser <|> jStringParser <|> jArrayParser <|> jObjectParser)
  <* wsParser

jNullParser :: Parser Json
jNullParser = const JNull <$> stringParser "null"

jBoolParser :: Parser Json
jBoolParser = jTrueParser <|> jFalseParser
  where jTrueParser = const (JBool True) <$> stringParser "true"
        jFalseParser = const (JBool False) <$> stringParser "false"

jNumberParser :: Parser Json
jNumberParser = (JNumber . read) <$> notNull (spanParser isDigit)

jStringParser :: Parser Json
jStringParser = JString <$> stringLitParser

jArrayParser :: Parser Json
jArrayParser = JArray <$> (charParser '[' *> itemsParser <* charParser ']')
  where itemsParser = separatedBy (stringParser ",") jsonParser

jObjectParser :: Parser Json
jObjectParser = JObject <$> (charParser '{' *> keyValPairsParser <* charParser '}')
  where keyValPairsParser = separatedBy (stringParser ",") pairParser
        pairParser = (,) <$> keyParser <* charParser ':' <*> jsonParser
        keyParser = wsParser *> stringLitParser <* wsParser

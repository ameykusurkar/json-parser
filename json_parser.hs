import Control.Applicative
import Data.Char

main = interact (show . fmap fst . runParser jsonParser)

data Json
 = JNull | JBool Bool | JNumber Int | JString String
 | JArray [Json] | JObject [(String, Json)]
 deriving (Show, Eq)

-- A parser that given an input string, consumes from it until it can form a value of
-- type `a`, and returns the remaining input wrapped in a Maybe. If it fails to form
-- an `a`, it returns Nothing. The idea is to build composable parsers that each look
-- for and form a specific value, and chain them.
newtype Parser a = Parser {
  runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
  fmap f (Parser p) = Parser q
    where q input = (p input) >>= applyF
          applyF (parsed, rest) = Just (f parsed, rest)

-- This is the core piece of logic that allows us to chain parsers. Implementing <*>
-- allows us to chain parsers together by passing the unused input from one parser to
-- the next one as input, and combine their parsed values.
instance Applicative Parser where
  pure x = Parser (\input -> Just (x, input))
  (Parser p) <*> (Parser q) = Parser r
    where r input = do (parsedF, rest) <- p input
                       (parsed, rest') <- q rest
                       return (parsedF parsed, rest')

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p) <|> (Parser q) = Parser (\input -> p input <|> q input)

-- Some helper parsers --

notNull :: Parser [a] -> Parser [a]
notNull (Parser f) = Parser p
  where p input = (f input) >>= fail
        fail (parsed, rest) = if null parsed then Nothing else Just (parsed, rest)

charParser :: Char -> Parser Char
charParser c = Parser f
  where f "" = Nothing
        f (c':cs)
          | c == c'   = Just (c, cs)
          | otherwise = Nothing

oneOfCharParser :: [Char] -> Parser Char
oneOfCharParser = foldl (<|>) empty . map charParser

stringParser :: String -> Parser String
stringParser = sequenceA . map charParser

intParser :: Parser Int
intParser = read <$> (unsignedIntParser <|> signedIntParser)
  where signedIntParser = (:) <$> charParser '-' <*> unsignedIntParser
        unsignedIntParser = notNull (spanParser isDigit)

spanParser :: (Char -> Bool) -> Parser String
spanParser f = Parser (Just . span f)

separatedBy :: Parser String -> Parser a -> Parser [a]
separatedBy sep item = (:) <$> item <*> many (sep *> item) <|> pure []

wsParser :: Parser String
wsParser = many (oneOfCharParser " \n\t")

stringLitParser :: Parser String
stringLitParser = charParser '"' *> spanParser (/= '"') <* charParser '"'

-- The parsing logic --

-- TODO: Parse string literals with escaped characters
--       Parse floating point numbers
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
jNumberParser = JNumber <$> intParser

jStringParser :: Parser Json
jStringParser = JString <$> stringLitParser

-- We can see here how small parsers can be chained to parse an array. We try to parse a
-- '[' character, followed one or many JSON items separated by a ',' and finally a '].
jArrayParser :: Parser Json
jArrayParser = JArray <$> (charParser '[' *> itemsParser <* charParser ']')
  where itemsParser = separatedBy (stringParser ",") jsonParser

jObjectParser :: Parser Json
jObjectParser = JObject <$> (charParser '{' *> keyValPairsParser <* charParser '}')
  where keyValPairsParser = separatedBy (stringParser ",") pairParser
        pairParser = (,) <$> keyParser <* charParser ':' <*> jsonParser
        keyParser = wsParser *> stringLitParser <* wsParser

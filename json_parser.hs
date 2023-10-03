import Control.Applicative
import Data.Foldable (fold)
import Data.Char (isDigit)

main = interact (show . fmap fst . runParser json)

data Json
 = JNull | JBool Bool | JNumber Int | JString String | JArray [Json] | JObject [(String, Json)]
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
    where q input = do (parsed, rest) <- p input
                       return (f parsed, rest)

-- This is the core piece of logic that allows us to chain parsers. Implementing <*>
-- allows us to chain parsers together by passing the unused input from one parser to
-- the next one as input, and combine their parsed values.
instance Applicative Parser where
  pure x = Parser (\input -> Just (x, input))
  (Parser p) <*> (Parser q) = Parser r
    where r input = do (parsedF, rest) <- p input
                       (parsed, rest') <- q rest
                       return (parsedF parsed, rest')

instance Semigroup a => Semigroup (Parser a) where
  p <> q = (<>) <$> p <*> q

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p) <|> (Parser q) = Parser (\input -> p input <|> q input)

-- Parse basic types --

charWhen :: (Char -> Bool) -> Parser Char
charWhen f = Parser p
  where p "" = Nothing
        p (c:cs)
          | f c       = Just (c, cs)
          | otherwise = Nothing

char :: Char -> Parser Char
char ch = charWhen (ch ==)

int :: Parser Int
int = read <$> (sign <> digits)
  where sign = optionalP $ string "-"
        digits = some $ charWhen isDigit

bool :: Parser Bool
bool = true <|> false
  where true = True <$ string "true"
        false = False <$ string "false"

optionalP :: Monoid a => Parser a -> Parser a
optionalP p = fold <$> optional p

string :: String -> Parser String
string = traverse char

oneOf :: (a -> Parser a) -> [a] -> Parser a
oneOf p = asum . map p

spanParser :: (Char -> Bool) -> Parser String
spanParser f = Parser (Just . span f)

-- TODO: Allow an optional final separator (eg. [1, 2,])
separated :: Parser b -> Parser a -> Parser [a]
separated by item = (:) <$> item <*> many (by *> item) <|> pure []

ws :: Parser String
ws = many (oneOf char " \n\t")

stringLiteral :: Parser String
stringLiteral = quote *> literal <* quote
  where quote = char '"'
        literal = spanParser (/= '"')

-- The parsing logic --

-- TODO: Parse string literals with escaped characters
--       Parse floating point numbers
json :: Parser Json
json = ws *> (jNull <|> jBool <|> jNumber <|> jString <|> jArray <|> jObject) <* ws
  where jNull = JNull <$ string "null"
        jBool = JBool <$> bool
        jNumber = JNumber <$> int
        jString = JString <$> stringLiteral

-- We can see here how small parsers can be chained to parse an array. We try to parse a
-- '[' character, followed one or many JSON items separated by a ',' and finally a '].
jArray :: Parser Json
jArray = JArray <$> (char '[' *> items <* char ']')
  where items = separated (string ",") json

jObject :: Parser Json
jObject = JObject <$> (char '{' *> pairs <* char '}')
  where pairs = separated (string ",") pair
        pair = (,) <$> key <* char ':' <*> json
        key = ws *> stringLiteral <* ws

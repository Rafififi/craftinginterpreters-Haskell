{-# LANGUAGE LambdaCase #-}
module CombinatorScanner(scanInput) where
import Control.Applicative
import Data.Char (isDigit, isAlphaNum)
import Token
import ParserDef

type Parser = GenParser String

char :: Char -> Parser Char
char x = Parser $ \case 
                  []     -> Nothing
                  (y:ys) -> if x == y then Just (ys, x) 
                                      else Nothing

string :: String -> Parser String
string = traverse char

span'  :: (Char -> Bool) -> Parser String
span'  f = Parser $ mySpan f
  where
    mySpan p (x:xs) 
      | p x       = Just (dropWhile p xs, x : takeWhile p xs)
      | otherwise = Nothing
    mySpan _ []   = Nothing

parseIf :: (Char -> Bool) -> Parser Char
parseIf f = Parser $ \case
           (x : xs) | f x -> Just (xs, x)
           _ -> Nothing

stringLiteral :: Parser TokenType
stringLiteral = STRING <$> (char '"' *> many (parseIf (\x -> x /= '"' && x /= '\\') <|> escapeChar) <* char '"') 
  where
    escapeChar = ('"'  <$ string "\\\"")
              <|>('\\' <$ string "\\\\")
              <|>('/'  <$ string "\\/")
              <|>('\b' <$ string "\\b")
              <|>('\f' <$ string "\\f")
              <|>('\n' <$ string "\\n")
              <|>('\r' <$ string "\\r")
              <|>('\t' <$ string "\\t")

double :: Parser TokenType
double = NUMBER <$> (((+) . (fromIntegral :: Int -> Double)
       .   read <$> digits)
      <*> (read . ('0' :) <$> ((:) <$> char '.' <*> digits) <|> pure 0))
  where digits = span' isDigit

comment :: Parser TokenType
comment =  COMMENT <$> (string "//" *> (span'  (/='\n') <|> pure "") <* (char '\n' <|> pure ' '))

identifier :: Parser TokenType
identifier =  IDENTIFIER <$> span'  (\x -> isAlphaNum x || x == '_')

singleCharToken :: Parser TokenType
singleCharToken =  (LEFTPAREN  <$ char '(')
               <|> (RIGHTPAREN <$ char ')')
               <|> (LEFTBRACE  <$ char '{')
               <|> (RIGHTBRACE <$ char '}')
               <|> (COMMA      <$ char ',')
               <|> (DOT        <$ char '.')
               <|> (MINUS      <$ char '-')
               <|> (PLUS       <$ char '+')
               <|> (SEMICOLON  <$ char ';')
               <|> (SLASH      <$ char '/')
               <|> (STAR       <$ char '*')
               <|> (BANG       <$ char '!')
               <|> (EQUAL      <$ char '=')
               <|> (GREATER    <$ char '>')
               <|> (LESS       <$ char '<')
               <|> (EOF        <$ char '\0')

doubleCharToken :: Parser TokenType
doubleCharToken =  (BANGEQUAL    <$ string "!=")
               <|> (EQUALEQUAL   <$ string "==")
               <|> (GREATEREQUAL <$ string ">=")
               <|> (LESSEQUAL    <$ string "<=")


keywords :: Parser TokenType
keywords =  (AND     <$ string "and" <* notFollowedByIdentifier)
        <|> (CLASS   <$ string "class" <* notFollowedByIdentifier)
        <|> (ELSE    <$ string "else" <* notFollowedByIdentifier)
        <|> (FUNCTION<$ string "fun" <* notFollowedByIdentifier)
        <|> (FOR     <$ string "for" <* notFollowedByIdentifier)
        <|> (IF      <$ string "if" <* notFollowedByIdentifier)
        <|> (NIL     <$ string "nil" <* notFollowedByIdentifier)
        <|> (OR      <$ string "or" <* notFollowedByIdentifier)
        <|> (PRINT   <$ string "print" <* notFollowedByIdentifier)
        <|> (RETURN  <$ string "return" <* notFollowedByIdentifier)
        <|> (SUPER   <$ string "super" <* notFollowedByIdentifier)
        <|> (THIS    <$ string "this" <* notFollowedByIdentifier)
        <|> (VAR     <$ string "var" <* notFollowedByIdentifier)
        <|> (WHILE   <$ string "while" <* notFollowedByIdentifier)
        <|> (BOOLEAN False <$ string "false" <* notFollowedByIdentifier)
        <|> (BOOLEAN True <$ string "true" <* notFollowedByIdentifier)

notFollowedByIdentifier :: Parser ()
notFollowedByIdentifier = Parser $ \case
  (x:_) | isAlphaNum x || x == '_' || x == '-' -> Nothing
  xs                                           -> Just (xs, ())

escapeChars :: Parser String
escapeChars =  string "\n"
           <|> string "\t"
           <|> string "\r"

whiteSpace :: Parser String
whiteSpace =  span' (==' ')

tokens :: Parser [TokenType]
tokens = many (discard *> token) <* discard
  where 
    discard = many (escapeChars <|> whiteSpace)
    token =  comment
         <|> stringLiteral 
         <|> doubleCharToken 
         <|> singleCharToken
         <|> double
         <|> keywords 
         <|> identifier

ignoreComments :: [TokenType] -> [TokenType]
ignoreComments (COMMENT _ : xs) = ignoreComments xs
ignoreComments (x:xs)           = x : ignoreComments xs
ignoreComments []               = []

scanInput :: String -> (String, [TokenType])
scanInput input = 
  case runParser tokens input of 
    Just ([], parsed)      -> ("", ignoreComments parsed)
    Just (unparsed, parsed)-> (unparsed, parsed)
    Nothing                -> ("", [])


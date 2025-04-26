{-# LANGUAGE LambdaCase #-}
module CombinatorParser(parseTokens) where
import Token
import AST
import Control.Applicative
import ParserDef

type Parser a = GenParser [TokenType] a

parseTokens :: [TokenType] -> [Expression] -> Maybe [Expression]
parseTokens [] acc     = Just $ reverse acc
parseTokens [EOF] acc  = Just $ reverse acc
parseTokens tokens acc = case runParser varDecl tokens of
  Just (unParsed, parsed) -> parseTokens unParsed (parsed : acc)
  Nothing                 -> Nothing

semiColon :: Parser TokenType
semiColon = getOp [SEMICOLON]

expression :: Parser Expression
expression = assignment 

getOp :: [TokenType] -> Parser TokenType
getOp tokens = Parser $ \case  
           (x : xs) | x `elem` tokens -> Just (xs, x)
           _ -> Nothing

printStatement :: Parser Expression
printStatement = (Print <$> (getOp [PRINT] *> expression <* semiColon)) <|> expressionStatement

expressionStatement :: Parser Expression
expressionStatement = ExprStmt <$> expression <* semiColon

assignment :: Parser Expression
assignment = (Assign <$> (equality <* getOp [EQUAL]) <*> assignment) <|> equality

equality :: Parser Expression
equality = binary [BANGEQUAL, EQUALEQUAL] comparison 

comparison :: Parser Expression
comparison  = binary [GREATEREQUAL, GREATER, LESS, LESSEQUAL] term

term :: Parser Expression
term = binary [MINUS, PLUS] factor

factor :: Parser Expression
factor = binary [SLASH, STAR] unary

binary :: [TokenType] -> Parser Expression ->  Parser Expression
binary tokens nextParser = Parser $ \input ->
  case runParser nextParser input of
    Just (input', left) -> loop left input'
    Nothing             -> Nothing
  where 
  loop expr curP = 
    case runParser (getOp tokens) curP of
      Nothing       -> Just (curP, expr)
      Just (p', op) ->
        case runParser nextParser p' of
          Just (p'', rightExpr) -> loop (Binary expr op rightExpr) p''
          Nothing               -> Nothing


unary :: Parser Expression
unary = (Unary <$> getOp [BANG, MINUS] <*> unary) <|> primary

varDecl :: Parser Expression
varDecl = Var <$> (getOp [VAR] *> identifier <* getOp [EQUAL]) <*> expressionStatement 
       <|>(Variable <$> (getOp [VAR] *> identifier <* semiColon))
       <|> printStatement

identifier :: Parser String
identifier = Parser (\case
               (IDENTIFIER x : xs) -> Just (xs, x)
               _                   -> Nothing)

primary :: Parser Expression
primary = (Literal <$> getOp [BOOLEAN False, BOOLEAN True, NIL]) 
       <|>(Literal <$> getOp [LEFTPAREN]) *> expression <* (Literal <$> getOp [RIGHTPAREN])
       <|> Literal . IDENTIFIER <$> identifier
       <|> Literal <$> Parser (\case
                         (NUMBER x: xs) -> Just (xs, NUMBER x)
                         (STRING x: xs) -> Just (xs, STRING x)
                         _              -> Nothing)

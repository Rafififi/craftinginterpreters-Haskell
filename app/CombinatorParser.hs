{-# LANGUAGE LambdaCase #-}
module CombinatorParser(parseTokens) where
import Token
import AST
import Control.Applicative
import ParserDef

type Parser a = GenParser [TokenType] a

parseTokens :: [TokenType] -> [Expression] -> ([TokenType], [Expression])
parseTokens [] acc     = ([], map desugarForLoop (reverse acc))
parseTokens [EOF] acc  = ([], map desugarForLoop (reverse acc))
parseTokens tokens acc = case runParser varDecl tokens of
  Just (unParsed, parsed) -> parseTokens unParsed (parsed : acc)
  Nothing                 -> (tokens, acc)

semiColon :: Parser TokenType
semiColon = getOp [SEMICOLON]


getOp :: [TokenType] -> Parser TokenType
getOp tokens = Parser $ \case  
           (x : xs) | x `elem` tokens -> Just (xs, x)
           _ -> Nothing

varDecl :: Parser Expression
varDecl = ( Var <$> (getOp [VAR] *> identifier <* getOp [EQUAL]) <*> expressionStatement)
       <|>(Variable <$> (getOp [VAR] *> identifier <* semiColon))
       <|> ifStatement

ifStatement :: Parser Expression
ifStatement =  (IfElse <$> (getOp [IF] *> getOp [LEFTPAREN] *> expression <* getOp [RIGHTPAREN]) <*> block <* getOp [ELSE] <*> block)
           <|> (If <$> (getOp [IF] *> getOp [LEFTPAREN] *> expression <* getOp [RIGHTPAREN]) <*> block)
           <|> printStatement

printStatement :: Parser Expression
printStatement = Print <$> (getOp [PRINT] *> expression <* semiColon) 
              <|> while

while :: Parser Expression
while =  While <$> (getOp [WHILE] *> getOp [LEFTPAREN] *> expression <* getOp [RIGHTPAREN]) <*> block
     <|> forStatements

forStatements :: Parser Expression
forStatements = For <$> (getOp [FOR] *> getOp [LEFTPAREN] *> initializer) 
             <*> optional expression <* semiColon 
             <*> optional expression <* getOp [RIGHTPAREN] 
             <*> block
             <|> block
  where
    initializer = (Nothing <$ semiColon) <|> optional (varDecl <|> expressionStatement)

desugarForLoop :: Expression -> Expression
desugarForLoop (For Nothing Nothing Nothing body) = Block [While trueLiteral body]
desugarForLoop (For Nothing Nothing (Just change) (Block body)) = Block [While trueLiteral (Block (body <> [Expr change]))]
desugarForLoop (For Nothing (Just cond) Nothing body) = Block [While cond body]
desugarForLoop (For (Just init') Nothing Nothing body) = Block [init', While trueLiteral body]
desugarForLoop (For Nothing (Just cond) (Just change) (Block body)) = Block [While cond (Block (body <> [Expr change]))]
desugarForLoop (For (Just init') Nothing (Just change) (Block body)) = Block [init', While trueLiteral (Block (body <> [Expr change]))]
desugarForLoop (For (Just init') (Just cond) Nothing body) = Block [init', While cond body]
desugarForLoop (For (Just init') (Just cond) (Just change) (Block body)) = Block [init', While cond (Block (body <> [Expr change]))]
desugarForLoop x = x

trueLiteral :: Expression
trueLiteral = Literal (BOOLEAN True)

block :: Parser Expression
block =  Block <$> (getOp [LEFTBRACE] *> many varDecl <* getOp [RIGHTBRACE]) 
     <|> expressionStatement

expressionStatement :: Parser Expression
expressionStatement = Expr <$> expression <* semiColon

expression :: Parser Expression
expression = assignment 

assignment :: Parser Expression
assignment = (Assign <$> (Variable <$> equality <* getOp [EQUAL]) <*> assignment) 
          <|> parseOr 

parseOr :: Parser Expression
parseOr = Logical <$> parseAnd <*> getOp [OR] <*> parseAnd
       <|> parseAnd

parseAnd :: Parser Expression
parseAnd = Logical <$> equality <*> getOp [AND] <*> equality
        <|> equality

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


identifier :: Parser Expression
identifier = Literal <$> Parser (\case
               (IDENTIFIER x : xs) -> Just (xs, IDENTIFIER x)
               _                   -> Nothing)

primary :: Parser Expression
primary = (Literal <$> getOp [BOOLEAN False, BOOLEAN True, NIL]) 
       <|>(Literal <$> getOp [LEFTPAREN]) *> expression <* (Literal <$> getOp [RIGHTPAREN])
       <|> identifier
       <|> Literal <$> Parser (\case
                         (NUMBER x: xs) -> Just (xs, NUMBER x)
                         (STRING x: xs) -> Just (xs, STRING x)
                         _              -> Nothing)

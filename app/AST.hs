module AST where
import Token
import Data.Map as M

type VarMap = Map String Expression

emptyMap :: VarMap
emptyMap = empty

define :: String -> Expression -> VarMap -> VarMap
define = insert 

get :: String -> VarMap -> Either String Expression
get name environment = case M.lookup name environment of
                        Nothing -> Left $ "Undefined Variable: " <> name <> "."
                        Just v  -> Right v

updateKey :: String -> Expression -> VarMap -> Either String Environment
updateKey name new vars =  case M.lookup name vars of
    Just _  -> Right $ Environment new (define name new vars)
    Nothing -> Left $ "Undefined Variable: " <> name <> "."

data Environment = Environment Expression VarMap

instance Show Environment where
  show (Environment expr _) = show expr

data Expression = Literal TokenType
                | Grouping Expression
                | Unary {
                    unOp    :: TokenType
                  , unRight :: Expression
                  }
                | Binary {
                    binLeft  :: Expression
                  , binOp    :: TokenType
                  , binRight :: Expression
                  }
                | Print Expression
                | ExprStmt Expression
                | Variable String 
                | Var String Expression
                | Assign Expression Expression
                deriving (Eq)

instance Show Expression where
  show (Literal token) = show token
  show (Variable token) = show "Variable: " <> show token
  show (Var token expr) = show token <> " = " <> show expr
  show (Assign token expr) = "Assign: " <> show token <> " = " <> show expr
  show (Grouping expr) = "(" <> show expr <> ")"
  show (Unary op right)= "(" <> show op <> " "  <> show right <> ")"
  show (Binary l op r) = "(" <> show l <> " " <> show op <> " "  <> show r <> ")"
  show (Print expr)    = "Print: " <> show expr
  show (ExprStmt expr) = "Expr: " <> show expr

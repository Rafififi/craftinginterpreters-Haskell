module Interpreter(evaluate) where
import AST
import Token

type IsPrint = Bool

evaluatePrint :: Environment -> Either String Expression
evaluatePrint (Environment (Print expr) vars) = case evaluateBinary (Environment expr vars) of
  Right res -> Right $ Literal $ STRING $ show res
  Left err  -> Left err
evaluatePrint (Environment (ExprStmt expr) vars) = evaluateBinary (Environment expr vars)
evaluatePrint env = evaluateBinary env

evaluateVar :: Environment -> Either String Environment
evaluateVar (Environment (Variable name) vars) = Right $ Environment (Var name (Literal NIL)) $ define name (Literal NIL) vars
evaluateVar (Environment (Var name expr) vars) = case evaluatePrint (Environment expr vars)of 
  Right res -> Right $ Environment (Var name res) $ define name res vars
  Left err  -> Left err
evaluateVar (Environment (Assign (Variable name) expr) vars) = case evaluatePrint (Environment expr vars) of
  Right res -> updateKey name res vars
  Left err  -> Left err
evaluateVar (Environment (Assign (Var    name _) expr) vars) = case evaluatePrint (Environment expr vars) of
  Right res -> updateKey name res vars
  Left err  -> Left err
evaluateVar (Environment (Assign _ _) _) = Left "Invalid Assignment Target"
evaluateVar (Environment expr env)= case evaluatePrint (Environment expr env) of
  Right res -> Right $ Environment res env
  Left err  -> Left err

evaluate :: Environment -> (Either String Environment, IsPrint)
evaluate (Environment expr vars) = case evaluateVar (Environment expr vars) of 
  Right binVal -> (Right binVal, isPrint expr)
  Left err     -> (Left err, isPrint expr)
  where isPrint (Print _) = True
        isPrint _         = False

isTruthy :: Expression -> Bool
isTruthy (Literal NIL) = False
isTruthy (Literal (BOOLEAN False)) = False
isTruthy _ = True

evaluateBinary :: Environment -> Either String Expression
evaluateBinary (Environment (Binary l BANGEQUAL r) vars) = case isEqual (evaluatePrint (Environment l vars)) (evaluatePrint (Environment r vars)) of
  Right res -> evaluateUnary $ Environment (Unary BANG res) vars
  Left err  -> Left err
evaluateBinary (Environment (Binary l EQUALEQUAL r) vars) = isEqual (evaluatePrint (Environment l vars)) (evaluatePrint (Environment r vars)) 
evaluateBinary (Environment (Binary left op right) vars) = case (evaluatePrint (Environment left vars) , evaluatePrint (Environment right vars)) of
  (Right (Literal (NUMBER x)), Right (Literal (NUMBER y))) -> case op of 
    MINUS        -> Right $ Literal $ NUMBER (x-y)
    SLASH        -> Right $ Literal $ NUMBER (x/y)
    PLUS         -> Right $ Literal $ NUMBER (x+y)
    STAR         -> Right $ Literal $ NUMBER (x*y)
    GREATER      -> Right $ Literal $ BOOLEAN (x > y)
    GREATEREQUAL -> Right $ Literal $ BOOLEAN (x >= y)
    LESS         -> Right $ Literal $ BOOLEAN (x < y)
    LESSEQUAL    -> Right $ Literal $ BOOLEAN (x <= y)
    _            -> Left "Right and LEFT are Numbers operation does not work on numbers"
  (Right (Literal (STRING x)), Right (Literal (STRING y))) -> case op of
    PLUS         -> Right $ Literal $ STRING (x <> y)
    _            -> Left "Right and right are strings operation is not +"
  (Right (Literal (BOOLEAN _)), Right (Literal (BOOLEAN _))) -> Left "Right and right are bools operation is not == or !="
  (Left err1, Left err2) -> Left $ "BIN OP ERROR: LEFT ERROR: " <> err1 <> " RIGHT ERROR: " <> err2
  (Left err1, _)         -> Left $ "BIN OP ERROR: left error: " <> err1 
  (_, Left err2)         -> Left $ "BIN OP ERROR: right error: " <> err2
  (Right r1, Right r2)   -> Left $ "Bin op: " <> show op <> " has incorrect types on left and right (" <> show r1 <> " and " <> show r2 <> ")"
evaluateBinary env = evaluateUnary env

isEqual :: Either String Expression -> Either String Expression -> Either String Expression 
isEqual (Left err1) (Left err2) = Left $ "BIN OP ERROR: LEFT ERROR: " <> err1 <> " RIGHT ERROR: " <> err2
isEqual (Left err1) _           = Left $ "BIN OP ERROR: left error: " <> err1 
isEqual _ (Left err2)           = Left $ "BIN OP ERROR: right error: " <> err2
isEqual l r = Right $ Literal $ BOOLEAN $ l == r

evaluateUnary :: Environment -> Either String Expression
evaluateUnary (Environment (Unary MINUS right) vars) = case evaluatePrint (Environment right vars) of
  Right (Literal (NUMBER x')) -> Right $ Literal $ NUMBER (-x')
  _                           -> Left "CAN NOT USE '-' on non number"
evaluateUnary (Environment (Unary BANG right) _) = Right $ Literal $ BOOLEAN $ not $ isTruthy right
evaluateUnary env = evaluateLiteral env 

evaluateLiteral :: Environment -> Either String Expression
evaluateLiteral (Environment (Literal (IDENTIFIER x)) vars) = get x vars
evaluateLiteral (Environment (Literal x) _)= Right $ Literal x
evaluateLiteral _ = Left "No literal at end of tree"

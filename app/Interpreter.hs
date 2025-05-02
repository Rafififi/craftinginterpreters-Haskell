module Interpreter(evaluate) where
import AST
import Token

type IsPrint = Bool
type IsBlock = Bool

evaluatePrint :: Environment -> Either String Expression
evaluatePrint (Environment (Print expr) vars) = case evaluateBinary (Environment expr vars) of
  Right res -> Right $ Literal $ STRING $ show res
  Left err  -> Left err
evaluatePrint (Environment (Expr expr) vars) = evaluateBinary (Environment expr vars)
evaluatePrint env = evaluateBinary env

evaluateVar :: Environment -> IsBlock -> [(Either String Environment, IsPrint)]
evaluateVar (Environment (Print (Assign var expr)) vars) isBlock = evaluateVar (Environment (Assign var expr) vars) isBlock 
evaluateVar (Environment (Expr (Assign var expr)) vars) isBlock = evaluateVar (Environment (Assign var expr) vars) isBlock
evaluateVar (Environment (Variable name) vars) isBlock = [(Right $ Environment (Var name (Literal NIL)) $ define name (Literal NIL) isBlock vars, False)]
evaluateVar (Environment (Var name expr) vars) isBlock = case evaluatePrint (Environment expr vars)of 
  Right res -> [(Right $ Environment (Var name res) $ define name res isBlock vars, False)]
  Left err  -> [(Left err, False)]
evaluateVar (Environment (Assign (Variable name) expr) vars) _ = case evaluatePrint (Environment expr vars) of
  Right res -> [(updateKey name res vars, isPrint expr)]
  Left err  -> [(Left err, False)]
evaluateVar (Environment (Assign (Var name _) expr) vars) _ = case evaluatePrint (Environment expr vars) of
  Right res -> [(updateKey name res vars, isPrint expr)]
  Left err  -> [(Left err, False)]
evaluateVar (Environment (Assign _ _) _) _ = [(Left "Invalid Assignment Target", False)]
evaluateVar (Environment expr vars) _ = evaluateWhile (Environment expr vars)

evaluateBlock :: [Expression] -> (VarMap, VarMap) -> [(Either String Environment, IsPrint)]
evaluateBlock [] _ = []
evaluateBlock ((Block statements):xs) (global, local) = evaluateBlock xs (global, local)
                                                       <> evaluateBlock statements (merge (global, local), emptyMap)
evaluateBlock (x:xs) (global, local) = case evaluateVar (Environment x (global, local)) True of
  []                           -> []
  [(Left err, _)]              -> [(Left err, True)]
  [(Right (Environment res vars), printVal)]-> evaluateBlock xs vars <> [(Right $ Environment res vars,printVal)]
  res                           -> res

isPrint :: Expression -> IsPrint
isPrint (Print _) = True
isPrint _         = False

evaluateIf :: Environment -> [(Either String Environment, IsPrint)]
evaluateIf (Environment (If cond (Block e1)) vars) = case evaluateBinary (Environment cond vars) of
  Left err     -> [(Left err, False)]
  Right isTrue -> if isTruthy isTrue 
                    then evaluateBlock e1 vars 
                    else [(Right $ Environment (Literal NIL) vars, False)]
evaluateIf (Environment (IfElse cond (Block e1) (Block e2)) vars) = case evaluateBinary (Environment cond vars) of
  Left err     -> [(Left err, False)]
  Right isTrue -> if isTruthy isTrue 
                    then  evaluateBlock e1 vars
                    else  evaluateBlock e2 vars
evaluateIf (Environment expr vars) = case evaluatePrint (Environment expr vars) of 
  Left err  -> [(Left err, isPrint expr)]
  Right res -> [(Right $ Environment res vars, isPrint expr)]

evaluateWhile :: Environment -> [(Either String Environment, IsPrint)]
evaluateWhile (Environment (While cond (Block body)) vars) = case evaluatePrint (Environment cond vars) of
                                                                Left err  -> [(Left err, False)]
                                                                Right res -> if isTruthy res
                                                                              then evaluateBody True vars
                                                                              else [(Right $ Environment (Literal NIL) vars, False)]
  where 
    evaluateBody False _ = [(Right $ Environment (Literal NIL) vars, False)]
    evaluateBody True vars' = do  
     let bodyRes = evaluateBlock body vars'
     case bodyRes of
      [] -> []
      xs -> case head xs of
        (Left err, _)                     -> [(Left err, False)]
        (Right (Environment _ vars''), _) -> case evaluatePrint $ Environment cond vars'' of 
                                                  Left err  -> [(Left err, False)]
                                                  Right res -> if isTruthy res
                                                                then evaluateBody True vars''  <> xs
                                                                else evaluateBody False vars'' <> xs

                                             
evaluateWhile env = evaluateIf env
     


evaluate :: Environment -> [(Either String Environment, IsPrint)]
evaluate (Environment (Block statments) vars) = reverse $ evaluateBlock statments vars
evaluate (Environment expr vars) = evaluateVar (Environment expr vars) False 

isTruthy :: Expression -> Bool
isTruthy (Literal NIL) = False
isTruthy (Literal (BOOLEAN False)) = False
isTruthy _ = True

evaluateBinary :: Environment -> Either String Expression
evaluateBinary (Environment (Logical l OR r) vars) = case evaluatePrint (Environment l vars) of
  Left err  -> Left err
  Right res -> if isTruthy res 
                then Right (Literal (BOOLEAN True))
                else case evaluatePrint (Environment r vars) of
                  Left err               -> Left err
                  Right res' -> Right $ Literal $ BOOLEAN $ isTruthy res' 
evaluateBinary (Environment (Logical l AND r) vars) = case evaluatePrint (Environment l vars) of
  Left err  -> Left err
  Right res -> if not $ isTruthy res
                then Right (Literal (BOOLEAN False))
                else case evaluatePrint (Environment r vars) of
                     Left err -> Left err
                     Right res' -> Right $ Literal $ BOOLEAN $ isTruthy res' 
      
evaluateBinary (Environment (Binary l BANGEQUAL r) vars) = case isEqual (evaluatePrint (Environment l vars)) (evaluatePrint (Environment r vars)) of
  Right res -> evaluateUnary $ Environment (Unary BANG res) vars
  Left err  -> Left err
evaluateBinary (Environment (Binary l EQUALEQUAL r) vars) = case isEqual (evaluatePrint (Environment l vars)) (evaluatePrint (Environment r vars)) of
  Right res -> Right res
  Left err  -> Left err
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
  (Left err1, Left err2) -> Left $ "BIN OP ERROR: LEFT ERROR: "  <> err1 <> " RIGHT ERROR: " <> err2
  (Left err1, _)         -> Left $ "BIN OP ERROR: left error: "  <> err1 
  (_, Left err2)         -> Left $ "BIN OP ERROR: right error: " <> err2
  (Right r1, Right r2)   -> Left $ "Bin op: " <> show op <> " has incorrect types on left and right (" <> show r1 <> " and " <> show r2 <> ")"
evaluateBinary env = evaluateUnary env

isEqual :: Either String Expression -> Either String Expression -> Either String Expression 
isEqual (Left err1) (Left err2) = Left $ "BIN OP ERROR: LEFT ERROR: "  <> err1 <> " RIGHT ERROR: " <> err2
isEqual (Left err1) _           = Left $ "BIN OP ERROR: left error: "  <> err1 
isEqual _ (Left err2)           = Left $ "BIN OP ERROR: right error: " <> err2
isEqual l r = Right $ Literal $ BOOLEAN $ l == r

evaluateUnary :: Environment -> Either String Expression
evaluateUnary (Environment (Unary MINUS right) vars) = case evaluatePrint (Environment right vars) of
  Right (Literal (NUMBER x')) -> Right $ Literal $ NUMBER (-x')
  _                           -> Left "CAN NOT USE '-' on non number"
evaluateUnary (Environment (Unary BANG right) _) = Right $ Literal $ BOOLEAN $ not $ isTruthy right
evaluateUnary env = evaluateLiteral env 

evaluateLiteral :: Environment -> Either String Expression
evaluateLiteral (Environment (Literal (IDENTIFIER x)) vars) = get (Literal $ IDENTIFIER x ) vars
evaluateLiteral (Environment (Literal x) _)= Right $ Literal x
evaluateLiteral _ = Left "No literal at end of tree"

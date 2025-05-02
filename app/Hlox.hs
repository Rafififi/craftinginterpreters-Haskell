module Hlox where
import System.Exit (exitWith, ExitCode (ExitFailure))
import CombinatorScanner
import System.IO (hFlush, stdout)
import CombinatorParser
import Interpreter
import AST
import Control.Monad (foldM)
type HadError = Bool

runner :: [String] -> IO ()
runner [file] = runFile file
runner []     = runPrompt emptyMap
runner _      = print "INCORRECT USAGE: HLOX [SOURCE]"

runPrompt :: VarMap -> IO ()
runPrompt vars = do
  putStr ">"
  hFlush stdout
  input <- getLine 
  case input of 
    []     -> putStrLn ""
    "\EOT" -> putStrLn ""
    ":q"   -> putStrLn ""  
    "list" -> print vars >> runPrompt vars
    _      -> do 
      (vars', _) <- run input vars
      runPrompt vars'

runFile :: String -> IO ()
runFile file = do
  source <- readFile file 
  failed <- fmap snd (run source emptyMap) 
  hadError failed
  where 
    hadError True  = exitWith (ExitFailure 65)
    hadError False = return ()

run :: String -> VarMap -> IO (VarMap, HadError)
run source vars = case scanInput source of
    ("", parsed)  -> case parseTokens parsed [] of
        ([], expressions)  -> print expressions >> hFlush stdout >> foldM evalAndCheck (vars, False) expressions 
        (unparsed, _)      -> print ("parsing Failed, Failed Here: " <> mconcat (map show $ take 10 unparsed)) >> return (vars, True)
    (unparsed, _) -> print unparsed >> return (vars, True)

evaluateHelper :: (Either String Environment, Bool) -> IO (Either () Environment)
evaluateHelper (Right res, True)  = print res >> return (Right res)
evaluateHelper (Right res, False) = return $ Right res
evaluateHelper (Left  err, True)  = print err >> return (Left ())
evaluateHelper (Left  err, False) = print err >> return (Left ())

evalAndCheck :: (VarMap, HadError) -> Expression -> IO (VarMap, HadError)
evalAndCheck (_, True) _ = return (emptyMap, True)
evalAndCheck (vars, False) expr = do
           res <- mapM evaluateHelper (evaluate (Environment expr (vars, emptyMap)))
           case res of
            []  -> return (vars, False)
            [x] -> case x of 
              Left _  -> return (emptyMap, True)
              Right (Environment _ (global,_)) -> return (global, False)
            xs -> case last xs of
              Left _  -> return (emptyMap, True)
              Right (Environment _ (global,_)) -> return (global, False)
              

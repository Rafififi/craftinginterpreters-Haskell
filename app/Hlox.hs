module Hlox where
import System.Exit (exitWith, ExitCode (ExitFailure))
import CombinatorScanner
import System.IO (hFlush, stdout)
import CombinatorParser
import Interpreter
import AST
import Control.Monad (foldM)
import Data.Map (empty)
type HadError = Bool

runner :: [String] -> IO ()
runner [file] = runFile file
runner []     = runPrompt
runner _      = print "INCORRECT USAGE: HLOX [SOURCE]"

runPrompt :: IO ()
runPrompt = do
  putStr ">"
  hFlush stdout
  input <- getLine 
  case input of 
    []     -> putStrLn ""
    "\EOT" -> putStrLn ""
    ":q"   -> putStrLn ""  
    _      -> run input >> runPrompt

runFile :: String -> IO ()
runFile file = readFile file >>= run >>= hadError
  where 
    hadError True  = exitWith (ExitFailure 65)
    hadError False = return ()

run :: String -> IO HadError
run source = case scanInput source of
    ("", parsed)  -> case parseTokens parsed [] of
        Nothing   -> print "parsing Failed" >> return True
        Just expressions -> print expressions >> snd <$> foldM evalAndCheck (empty, False) expressions 
    (unparsed, _) -> print unparsed >> return True

evaluateHelper :: (Either String Environment, Bool) -> IO (Either () Environment)
evaluateHelper (Right res, True)  = print res >> return (Right res)
evaluateHelper (Right res, False) = return $ Right res
evaluateHelper (Left  err, True)  = print err >> return (Left ())
evaluateHelper (Left  err, False) = print err >> return (Left ())

evalAndCheck :: (VarMap, HadError) -> Expression -> IO (VarMap, HadError)
evalAndCheck (_, True) _ = return (empty, True)
evalAndCheck (vars, False) expr = res
  where res = do
           res' <- evaluateHelper (evaluate (Environment expr vars))
           case res' of
            Left _ -> return (empty, True)
            Right (Environment _ vars') -> print vars' >> return (vars', False)

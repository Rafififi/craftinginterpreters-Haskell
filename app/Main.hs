module Main where
import Hlox
import System.Environment (getArgs)

main :: IO ()
main = do
  putStrLn ""
  runner =<< getArgs

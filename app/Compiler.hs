-- The Compiler file (we don't need a module declaration here!)
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Text.Parsec.String
import Control.Monad
import Parser
import Sprockell
import Generator
import Structure
import Text.Read

main = do args <- getArgs
          commandParser args

commandParser :: [String] -> IO()
commandParser ["c", file, tCount] = generateAndRun file (readMaybe tCount)
commandParser ["ast", file] = printAST file
commandParser ["gen", file, tCount] = printSpril file (readMaybe tCount)
commandParser _ = putStrLn $ "AMv compiler usage: c <file> <threadAmount> (compile and run file)\n"
                          ++ "                  |ast <file> (get the AST of file)\n"
                          ++ "                  |gen <file> <threadAmount> (print sprockell code of file)"

generateAndRun :: String -> Maybe Int -> IO ()
generateAndRun file (Just tCount) = do
  result <- parseFromFile parseBlock file
  case result of
    Left err -> print err
    Right xs -> run $ replicate tCount $ generation xs tCount
generateAndRun _ _ = error "Please specify the amount of threads as an Int"

printAST :: String -> IO ()
printAST file = do
  result <- parseFromFile parseBlock file
  case result of
    Left err -> print err
    Right xs -> print xs

printSpril :: String -> Maybe Int -> IO ()
printSpril file (Just tCount) = do
  result <- parseFromFile parseBlock file
  case result of
    Left err -> print err
    Right xs -> putStrLn $ pretty $ generation xs tCount
printSpril _ _ = error "Please specify the amount of threads as an Int"

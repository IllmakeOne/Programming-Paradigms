-- The Compiler file (we don't need a module declaration here!)
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Text.Parsec.String
import Control.Monad
import Parser


main = do args <- getArgs
          case length args of
               0 -> askForFile
               2-> command args
               otherwise -> putStrLn "AMV compiler takes only 0 or 2 arguments"

command :: [String] -> IO ()
command args = do
  case args !! 0 of
    "ast" -> makeAST $ args !! 1
    otherwise -> putStrLn "ALRIGHT WE're going to compile (not implemented yet)"

makeAST :: String -> IO ()
makeAST file = do
  result <- parseFromFile parseBlock file
  case result of
    Left err -> print err
    Right xs -> print xs


askForFile :: IO ()
askForFile = putStrLn "Je moeder"



-- -- openFileWithName :: String -> IO Handle
-- openFileWithName fileName = openFile fileName ReadMode
--
-- -- getFileContents :: String -> IO String
-- getFileContents fileName = hGetContents $ openFileWithName fileName

-- compile :: String -> IO ()
-- compile file = do
--   handle <- openFile file ReadMode
--   contents <- hGetContents handle
--   putStr contents



-- main :: IO ()
-- main args | length args == 0 = askForFile
--           | length args == 1 = compile
--           where
--             args = getArgs





-- flushStr :: String -> IO ()
-- flushStr str = putStr str >> hFlush stdout
--
-- readPrompt :: String -> IO String
-- readPrompt prompt = flushStr prompt >> getLine
--
-- until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
-- until_ pred prompt action = do
--   result <- prompt
--   if pred result
--      then return ()
--      else action result >> until_ pred prompt action
--
-- evalAndPrint :: String -> IO ()
-- evalAndPrint expr =  evalString expr >>= putStrLn
--
-- runRepl :: IO ()
-- runRepl = until_ (== "quit") (readPrompt "AMV>>> ") evalAndPrint

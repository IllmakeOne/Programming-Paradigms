-- The Compiler file (we don't need a module declaration here!)
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad


main = do args <- getArgs
          case length args of
               0 -> askForFile
               1 -> compile $ args !! 0
               otherwise -> putStrLn "AMV compiler takes only 0 or 1 argument"

askForFile :: IO ()
askForFile = putStrLn "Je moeder"



-- -- openFileWithName :: String -> IO Handle
-- openFileWithName fileName = openFile fileName ReadMode
--
-- -- getFileContents :: String -> IO String
-- getFileContents fileName = hGetContents $ openFileWithName fileName

compile :: String -> IO ()
compile file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  putStr contents



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

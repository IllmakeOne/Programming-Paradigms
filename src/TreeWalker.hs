module TreeWalker where

import Parser
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
-- import Data



-- typeChecker :: Bloc
-- typeChecker (Bloc (VarDecl (Bol name) ) )

data Function = DBFunction ArgType [ArgType]
    deriving Show


fromBlock :: Bloc -> [Commands]
fromBlock (Block xs) = xs

-- scopes :: Bloc -> [[ArgType]]
-- scopes (Block xs) db =

-- scopes :: Bloc -> [[ArgType]]
-- scopes x = []
-- scopes block = p args
--       where
--         arg = fromBlock block
--         p ((VarDecl arg expr ):xs) =




functions :: [Commands] -> [Function]
functions [] = []
functions ((FunDecl a args irel):xs) = (DBFunction a args) : functions xs
functions (x:xs) = functions xs
functions_test1 = functions []






























--eof

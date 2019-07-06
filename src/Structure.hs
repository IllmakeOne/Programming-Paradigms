module Structure where



data Bloc = Block [Commands]
      deriving (Eq,Show)


data Commands = VarDecl ArgType Expr
              | GlobalVarDecl ArgType Expr
              | FunDecl ArgType [Param] Bloc
              | FunCall String [Expr]
              | Fork Expr
              | Join
              | Print Expr
              | Ass String Expr
              | IfCom Condition Bloc Bloc
              | While Condition Bloc
              | Incr String
              | Decr String
              | AddCom String Expr
              | MinCom String Expr
              | Comment String      --TODO use endby
              | Nop
              | End
              | Return Expr
            deriving (Eq,Show)

data Param = ByVal ArgType | ByRef ArgType
      deriving (Eq,Show)

data ArgType = Arg Type String -- Bol String | Int String | Void String
      deriving (Eq,Show)

data Type = SimplyBol | SimplyInt | SimplyNull
      deriving (Eq,Show)


data Expr = Constant Integer -- DONE IMPLEMENTED IN GENERATOR
            | BoolConst Bool
            | Identifier String
            | Mult Expr Expr
            | Add Expr Expr
            | Min Expr Expr
            | Funct String [Expr]
            | IfExpr Type Condition Expr Expr
            | Paren Expr
            | NullExpr
            deriving (Eq,Show)

data Condition = Lt Expr Expr -- DONE IMPLEMENTED IN GENERATOR
         | Eq Expr Expr -- DONE IMPLEMENTED IN GENERATOR
         | Gt Expr Expr -- DONE IMPLEMENTED IN GENERATOR
         | Lq Expr Expr -- DONE IMPLEMENTED IN GENERATOR
         | Gq Expr Expr -- DONE IMPLEMENTED IN GENERATOR
      deriving (Eq,Show)



fromBlock :: Bloc -> [Commands]
fromBlock (Block a) =  a

typeArgtype:: ArgType -> Type
typeArgtype (Arg t _ ) = t

stringArtgType :: ArgType ->  String
stringArtgType (Arg _ x ) = x

fromRight :: b -> Either a b -> b
fromRight b (Left _) = b
fromRight _ (Right b) = b

isLeft :: Either a b -> Bool
isLeft(Left _ ) = True
isLeft(Right _) = False


-----------------Errors-------------------------------
data TypeError = Er String | Ok | Crt Type
      deriving (Eq,Show)



boolTypeError :: TypeError -> Bool
boolTypeError (Ok)= True
boolTypeError (Crt _ )= True
boolTypeError (Er _ )= False

getType :: TypeError -> Type
getType (Crt a) =  a
getType (Ok ) = SimplyNull
getType (Er _ ) = SimplyNull


stringTypeError (Er message ) = message

addMessage ::String -> TypeError ->TypeError
addMessage soure (Er message) = Er (message ++ " in " ++soure)
addMessage soure (Ok) = Er soure
addMessage soure (Crt _ ) = Er soure

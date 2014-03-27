module Parser 
    ( Stmt(..)
    , BinOp(..)
    , Expr(..)
    , parseProgram )
where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Monad

data Stmt = Assignment String Expr
          | Print Expr
          | IfThen Expr Stmt
          | While Expr Stmt
          | Block [Stmt]
    deriving (Eq, Show)

data BinOp = Add | Subtract | Multiply | Divide | Div | Mod
    deriving (Eq, Show)

data Expr = BinExpr BinOp Expr Expr
          | Id String
          | Const Integer
    deriving (Eq, Show)

languageDef = 
    emptyDef { Token.identStart = letter
             , Token.identLetter = alphaNum
             , Token.reservedNames = [ "if"
                                     , "then"
                                     , "while"
                                     , "do"
                                     , "print"
                                     , "begin"
                                     , "end" ]
             , Token.reservedOpNames = [ "+", "-", "*", "/"
                                       , "div", "mod" ]
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

operators = map toParsecOp [ ("*", Multiply), ("/", Divide), ("div", Div)
                           , ("mod", Mod), ("+", Add), ("-", Subtract) ]
    where toParsecOp (s, o) = [Infix ((reservedOp s) >> return (BinExpr o)) AssocLeft]

expr :: Parser Expr
expr = buildExpressionParser operators term

term :: Parser Expr
term = parens expr
    <|> liftM Id identifier
    <|> liftM Const integer

assignmentStmt :: Parser Stmt
assignmentStmt = do
    lval <- identifier
    reserved ":="
    rhs <- expr
    return $ Assignment lval rhs

printStmt :: Parser Stmt
printStmt = do
    reserved "print"
    val <- expr
    return $ Print val

ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    cond <- expr
    reserved "then"
    body <- stmt
    return $ IfThen cond body

whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    cond <- expr
    reserved "do"
    body <- stmt
    return $ While cond body

stmtBlock :: Parser Stmt
stmtBlock = do
    reserved "begin"
    body <- sepBy1 stmt semi
    reserved "end"
    return $ Block body

stmt :: Parser Stmt
stmt = assignmentStmt
    <|> printStmt
    <|> ifStmt
    <|> whileStmt
    <|> stmtBlock

program :: Parser Stmt
program = whiteSpace >> stmt

parseProgram :: String -> Stmt
parseProgram str = case parse program "" str of
    Left e -> error $ show e
    Right r -> r

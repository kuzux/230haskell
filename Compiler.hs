module Main where

import Parser
import Command
import Control.Monad
import System.Environment
import System.FilePath

toCommandFormat :: Command -> String
toCommandFormat (Operator Add) = "+"
toCommandFormat (Operator Subtract) = "-"
toCommandFormat (Operator Multiply) = "*"
toCommandFormat (Operator Divide) = "/"
toCommandFormat (Operator Div) = "div"
toCommandFormat (Operator Mod) = "mod"
toCommandFormat (Push n) = "push " ++ (show n)
toCommandFormat (Lvalue s) = "lvalue " ++ s
toCommandFormat (Rvalue s) = "rvalue " ++ s
toCommandFormat (Pop) = "pop"
toCommandFormat (Assign) = ":="
toCommandFormat (Copy) = "copy"
toCommandFormat (PrintCmd) = "print"
toCommandFormat (Label s) = "label " ++ s
toCommandFormat (Goto s) = "goto " ++ s
toCommandFormat (GoTrue s) = "gotrue " ++ s
toCommandFormat (GoFalse s) = "gofalse " ++ s
toCommandFormat (Halt) = "halt"

compileExpr :: Expr -> [Command]
compileExpr (Id s) = [ Rvalue s ]
compileExpr (Const i) = [ Push i ]
compileExpr (BinExpr op left right) = compileExpr left ++ compileExpr right ++ [ Operator op ]

compileStmt :: Stmt -> Int -> ([Command], Int)
compileStmt (Assignment l r) n  = ([ Lvalue l ] ++ compileExpr r ++ [ Assign ], n)
compileStmt (Print v) n         = (compileExpr v ++ [ PrintCmd ], n)
compileStmt (Block xs) n        = foldl stmtBlock ([], n) xs
    where stmtBlock (cmds, labelNo) stmt = (cmds ++ newCmds, newN) 
            where (newCmds, newN) = compileStmt stmt labelNo
compileStmt (IfThen cond body) n = (compileExpr cond ++ [ GoFalse (show n) ] ++ bodyCmds ++ [ Label (show n) ], newN )
    where (bodyCmds, newN) = compileStmt body (n+1)
compileStmt (While cond body) n = ([Label (show n)] ++ compileExpr cond ++ [ GoFalse (show $ n+1)] ++ bodyCmds ++ [ Goto (show n) ] ++ [ Label (show $ n+1) ], newN)
    where (bodyCmds, newN) = compileStmt body (n+2)

compileProgram :: String -> String
compileProgram s = unlines $ map toCommandFormat $ cmds ++ [ Halt ]
    where (cmds, _) = (flip compileStmt) 0 $ parseProgram s 

compileFile :: FilePath -> IO String
compileFile = (liftM compileProgram) . readFile

main :: IO ()
main = do (filename:_) <- getArgs
          res <- compileFile filename
          let outName = replaceExtension filename "m"
          writeFile outName res

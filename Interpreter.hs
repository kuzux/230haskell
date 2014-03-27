module Main where

import Command
import Parser (BinOp(..))
import Control.Monad
import System.Environment
import Data.IORef

toCommandList :: String -> [Command]
toCommandList str = map (toCommand . words) $ lines str
    where toCommand ("+":_) = Operator Add
          toCommand ("-":_) = Operator Subtract
          toCommand ("*":_) = Operator Multiply
          toCommand ("/":_) = Operator Divide
          toCommand ("div":_) = Operator Div
          toCommand ("mod":_) = Operator Mod
          toCommand ("push":n:_) = Push (read n)
          toCommand ("lvalue":s:_) = Lvalue s
          toCommand ("rvalue":s:_) = Rvalue s
          toCommand ("pop":_) = Pop
          toCommand (":=":_) = Assign
          toCommand ("copy":_) = Copy
          toCommand ("print":_) = PrintCmd
          toCommand ("label":s:_) = Label s
          toCommand ("goto":s:_) = Goto s
          toCommand ("gotrue":s:_) = GoTrue s
          toCommand ("gofalse":s:_) = GoFalse s
          toCommand ("halt":_) = Halt
          toCommand _ = error "Invalid operation"

interpret :: [Command] -> IO()


main :: IO ()
main = do 
    (filename:_) <- getArgs
    res <- (liftM toCommandList) $ readFile filename
    putStrLn (show res)

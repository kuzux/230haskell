module Main where

import Command
import Parser (BinOp(..))
import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import qualified Data.Map as M
import qualified Data.Vector as V

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


data Env = Env { stack  :: [Double]
               , code   :: V.Vector Command
               , pc     :: Int
               , vars   :: M.Map String Double
               , labels :: M.Map String Int 
               }
type Exec a = Identity a   -- will become a transformer stack, one day :)

runExec :: Exec a -> a
runExec = runIdentity

exec :: Env -> Exec Double
exec = undefined

initialEnvironment :: [Command] -> Env
initialEnvironment = undefined

main :: IO ()
main = do 
    (filename:_) <- getArgs
    res <- (liftM toCommandList) $ readFile filename
    putStrLn (show res)

module Main where

import Command
import Parser (BinOp(..))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import System.Environment
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Maybe

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


data Env = Env { stack       :: [Double]
               , code        :: V.Vector Command
               , pc          :: Int
               , varNames    :: M.Map String Int
               , vars        :: V.Vector Double
               , labels      :: M.Map String Int
               , running     :: Bool
               , printBuffer :: Maybe String   -- this is required in order to keep execCommand pure
               } deriving (Show)
type Exec a = Identity a   -- will become a transformer stack, one day :)

runExec :: Exec a -> a
runExec = runIdentity

exec :: Env -> Exec Env
exec env | running env == False = return env
         | otherwise            = exec $ execCommand env currCommand
  where currCommand = (code env) V.! (pc env)

execCommand :: Env -> Command -> Env
execCommand env (Operator Add)      = nextOp $ execBinOp env (+)
execCommand env (Operator Subtract) = nextOp $ execBinOp env (-)
execCommand env (Operator Multiply) = nextOp $ execBinOp env (*)
execCommand env (Operator Divide)   = nextOp $ execBinOp env (/)
execCommand env (Operator Div)      = nextOp $ execIntegerBinOp env div
execCommand env (Operator Mod)      = nextOp $ execIntegerBinOp env mod
execCommand env (Push n)            = nextOp $ modifyStack env 0 $ \ _ -> [fromIntegral n]
execCommand env (Pop)               = nextOp $ modifyStack env 1 $ \ _ -> []
execCommand env (Copy)              = nextOp $ modifyStack env 1 $ \ (a:_) -> [a,a]
execCommand env (Halt)              = env { running = False }
execCommand env (Goto s)            = env { pc = fromJust $ M.lookup s (labels env) }
execCommand env (GoTrue s)          
    | (head . stack $ env) /= 0 = execCommand (modifyStack env 1 $ \ _ -> []) (Goto s)
    | otherwise                 = execCommand env Pop
execCommand env (GoFalse s)          
    | (head . stack $ env) == 0 = execCommand (modifyStack env 1 $ \ _ -> []) (Goto s)
    | otherwise                 = execCommand env Pop
execCommand env (Label s)           = nextOp env
execCommand env (PrintCmd)          = nextOp $ env { printBuffer = Just ( show . head . stack $ env)
                                                   , stack = (tail . stack $ env ) }
execCommand env (Lvalue s)          = nextOp $ modifyStack env 0 $ \ _ -> [ addr ]
    where addr = fromIntegral . fromJust $ M.lookup s (varNames env)
execCommand env (Rvalue s)          = nextOp $ modifyStack env 0 $ \ _ -> [ value ]
    where value = (vars env) V.! (fromJust $ M.lookup s (varNames env))
execCommand env (Assign)            = nextOp $ env { stack = rest, vars = newVars }
    where (val:addr:rest) = stack env
          newVars = (vars env) V.// [(truncate addr, val)] 

nextOp :: Env -> Env
nextOp env = env { pc = pc env + 1 }

-- takes an environment and number of items to be popped
modifyStack :: Env -> Int -> ([Double] -> [Double]) -> Env
modifyStack env n f = env { stack = (f (take n $ stack env)) ++ (drop n $ stack env) }

execBinOp :: Env -> (Double -> Double -> Double) -> Env
execBinOp env f = modifyStack env 2 $ \ (b:a:_) -> [a `f` b]

execIntegerBinOp :: Env -> (Integer -> Integer -> Integer) -> Env
execIntegerBinOp env f = execBinOp env ( \ a b -> fromIntegral $ (truncate a) `f` (truncate b)) 

withIndices :: [a] -> [(Int, a)]
withIndices = zip [0..]

initialEnvironment :: [Command] -> Env
initialEnvironment cmds = Env { stack       = []
                              , code        = V.fromList cmds
                              , pc          = 0
                              , labels      = extractLabels cmds
                              , varNames    = varnames
                              , vars        = V.fromList $ replicate (M.size varnames) 0.0
                              , running     = True
                              , printBuffer = Nothing
                              }
    where varnames = extractVars cmds

          extractVars :: [Command] -> M.Map String Int
          extractVars cmds = snd $ foldl extractVars' (0, M.empty) cmds
          
          extractVars' :: (Int, M.Map String Int) -> Command -> (Int, M.Map String Int)
          extractVars' (i, vars) (Lvalue s) | M.member s vars = (i, vars)
                                            | otherwise       = (i+1, M.insert s i vars)
          extractVars' (i, vars) _          = (i, vars)

          extractLabels :: [Command] -> M.Map String Int
          extractLabels cmds = foldl extractLabels' M.empty (withIndices cmds)
          
          extractLabels' :: M.Map String Int -> (Int, Command) -> M.Map String Int
          extractLabels' labels (idx, Label s) = M.insert s idx labels
          extractLabels' labels _              = labels

main :: IO ()
main = do 
    (filename:_) <- getArgs
    cmds <- (liftM toCommandList) $ readFile filename
    putStrLn . show . runExec . exec . initialEnvironment $ cmds

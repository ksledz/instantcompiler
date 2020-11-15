module LLVMCompiler (allToLLVM) where

import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State

import AbsInstant


pre :: String
pre = "declare i32 @printf(i8*, ...) \n @.fmt = constant [4 x i8] c\"%d\\0A\\00\" \n define i32 @main() { \n "
post :: String
post = "ret i32 0 \n }"

type LLVMonad a = ReaderT (Map Ident String) (State Int) a

freshRegister :: LLVMonad String
freshRegister = do
  oldRegister <- get
  put $ oldRegister + 1
  return $ "%reg" ++ show (oldRegister + 1)

stmtsToLLVM :: [Stmt] -> LLVMonad String

stmtsToLLVM (SAss string aexpr:rest) = do
  (reg, code) <- aexprToLLVM aexpr
  llvmRest <- local (Map.insert string reg) $ stmtsToLLVM rest
  return $ code ++ llvmRest

stmtsToLLVM (SExp aexpr:rest) = do
  (reg, code) <- aexprToLLVM aexpr
  llvmRest <-  stmtsToLLVM rest
  return $ code ++ "call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.fmt, i32 0, i32 0), i32 " ++ reg ++ ")\n" ++ llvmRest

stmtsToLLVM ([]) = return ""

aexprToLLVM :: Exp -> LLVMonad (String, String)

aexprToLLVM (ExpVar string) =  do
  var <- ask
  return (fromJust (Map.lookup string var), "")

aexprToLLVM (ExpLit integer) = return (show integer, "")

aexprToLLVM (ExpAdd ex1 ex2) = opToLLVM ex1 ex2 "add"
aexprToLLVM (ExpMul ex1 ex2) = opToLLVM ex1 ex2 "mul"
aexprToLLVM (ExpSub ex1 ex2) = opToLLVM ex1 ex2 "sub"
aexprToLLVM (ExpDiv ex1 ex2) = opToLLVM ex1 ex2 "sdiv"

opToLLVM :: Exp -> Exp -> String -> LLVMonad (String, String)
opToLLVM ex1 ex2 op = do
  (reg1, code1) <- aexprToLLVM ex1
  (reg2, code2) <- aexprToLLVM ex2
  res <- freshRegister
  return (res, code1 ++ code2 ++ res ++ " = " ++ op ++" i32 " ++ reg1 ++ " , " ++ reg2 ++ "\n")

allToLLVM :: Program -> String
allToLLVM (Prog stmts) = pre ++ evalState(runReaderT (stmtsToLLVM stmts) Map.empty) 0 ++ post

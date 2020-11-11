module LLVMCompiler (allToLLVM) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Char as Char
import Control.Monad (void)
import Control.Monad.Reader
import Control.Monad.State
import Data.Void

import AbsInstant


pre :: String
pre = "declare void @printInt(i32) ; \n define i32 @main() { \n "
post :: String
post = "ret i32 0 \n }"

type LLVMonad a = ReaderT (Map Ident String) (State Int) a

freshRegister :: LLVMonad String
freshRegister = do
  oldRegister <- get
  put $ oldRegister + 1
  return $ "%XD" ++ show (oldRegister + 1)

stmtsToLLVM :: [Stmt] -> LLVMonad String

stmtsToLLVM (SAss string aexpr:rest) = do
  (reg, code) <- aexprToLLVM aexpr
  varName <- freshRegister
  llvmRest <- local (Map.insert string varName) $ stmtsToLLVM rest
  return $ code ++ varName ++ " = " ++ reg ++ "\n " ++ llvmRest

stmtsToLLVM (SExp aexpr:rest) = do
  (reg, code) <- aexprToLLVM aexpr
  llvmRest <-  stmtsToLLVM rest
  return $ code ++ "call void @printInt(i32 " ++ reg ++ ")\n " ++ llvmRest

stmtsToLLVM ([]) = return ""

aexprToLLVM :: Exp -> LLVMonad (String, String)

aexprToLLVM (ExpVar string) =  do
  var <- ask

  return (Map.findWithDefault "" string var, "")

aexprToLLVM (ExpLit integer) = return (show integer, "")

aexprToLLVM (ExpAdd ex1 ex2) = opToLLVM ex1 ex2 "add"
aexprToLLVM (ExpMul ex1 ex2) = opToLLVM ex1 ex2 "mul"
aexprToLLVM (ExpSub ex1 ex2) = opToLLVM ex1 ex2 "sub"
aexprToLLVM (ExpDiv ex1 ex2) = opToLLVM ex1 ex2 "div"

opToLLVM :: Exp -> Exp -> String -> LLVMonad (String, String)
opToLLVM ex1 ex2 op = do
  (reg1, code1) <- aexprToLLVM ex1
  (reg2, code2) <- aexprToLLVM ex2
  res <- freshRegister
  return (res, code1 ++ code2 ++ res ++ " = " ++ op ++" " ++ reg1 ++ " " ++ reg2 ++ "\n ")

allToLLVM :: Program -> String
allToLLVM (Prog stmts) = pre ++ evalState(runReaderT (stmtsToLLVM stmts) Map.empty) 0 ++ post

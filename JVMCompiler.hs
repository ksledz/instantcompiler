module JVMCompiler (allToJVM) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Char as Char
import Control.Monad (void)
import Control.Monad.Reader
import Control.Monad.State
import Data.Void

import AbsInstant


pre :: String -> String
pre fname = ".class  public " ++ fname ++ "\n" ++
  ".super  java/lang/Object\n" ++
  ".method public <init>()V\n" ++
  "aload_0\n" ++
  "invokespecial java/lang/Object/<init>()V\n" ++
  "return\n" ++
  ".end method\n" ++
  ".method public static main([Ljava/lang/String;)V\n"
post :: String
post = "return \n.end method"

type JVMonad a = ReaderT (Integer) (State (Map Ident Integer)) a

pushConst :: Integer -> String
pushConst i
  | i <= 5 = "iconst_" ++ show i
  | i < 128 = "bipush " ++ show i
  | i < 32768 = "sipush " ++ show i
  | otherwise = "ldc " ++ show i

pushVar :: Integer -> String
pushVar i
  | i <= 3 = "iload_" ++ show i
  | otherwise = "iload " ++ show i

storeVar :: Integer -> String
storeVar i
  | i <= 3 = "istore_" ++ show i
  | otherwise = "istore " ++ show i


printStream :: String
printStream = "getstatic  java/lang/System/out Ljava/io/PrintStream;\n"
invokeVirtual :: String
invokeVirtual = "invokevirtual  java/io/PrintStream/println(I)V\n"

stmtsToJVM :: [Stmt] -> JVMonad String

stmtsToJVM (SAss string aexpr:rest) = do
  code <- aexprToJVM aexpr
  map <- get
  when (Map.member string map)$ modify (Map.insert string (toInteger $ Map.size map))
  let id = Map.findWithDefault 0 string map
  jvmRest <- stmtsToJVM rest
  return $ code ++ storeVar id ++"\n" ++ jvmRest

stmtsToJVM (SExp aexpr:rest) = do
  code <- aexprToJVM aexpr
  jvmRest <-  stmtsToJVM rest
  return $ printStream ++ code ++ invokeVirtual ++ jvmRest

stmtsToJVM ([]) =
  return ""

aexprToJVM :: Exp -> JVMonad (String)

aexprToJVM (ExpVar string) =  do
  var <- get
  let i = Map.findWithDefault 0 string var
  return $ pushVar i ++ "\n"

aexprToJVM (ExpLit integer) = return (pushConst integer ++ "\n")

aexprToJVM (ExpAdd ex1 ex2) = opToJVM ex1 ex2 "add"
aexprToJVM (ExpMul ex1 ex2) = opToJVM ex1 ex2 "mul"
aexprToJVM (ExpSub ex1 ex2) = opToJVM ex1 ex2 "sub"
aexprToJVM (ExpDiv ex1 ex2) = opToJVM ex1 ex2 "div"

opToJVM :: Exp -> Exp -> String -> JVMonad (String)
opToJVM ex1 ex2 op = do
  code1 <- aexprToJVM ex1
  code2 <- aexprToJVM ex2
  return $ code1 ++ code2 ++ " i" ++ op ++"\n"

getOpLimit :: Exp -> Exp -> Integer
getOpLimit e1 e2 =
  let e1L = getExpLimit e1 in let e2L = getExpLimit e2 in max e1L (1 + e2L)

getExpLimit :: Exp -> Integer
getExpLimit (ExpVar _) = 1
getExpLimit (ExpLit _) = 1
getExpLimit (ExpAdd e1 e2) = getOpLimit e1 e2
getExpLimit (ExpMul e1 e2) = getOpLimit e1 e2
getExpLimit (ExpSub e1 e2) = getOpLimit e1 e2
getExpLimit (ExpDiv e1 e2) = getOpLimit e1 e2

getStmtLimit :: Stmt -> Integer
getStmtLimit (SAss _ exp) = getExpLimit exp
getStmtLimit (SExp exp) = 1 + getExpLimit exp

getLimit :: [Stmt] -> Integer
getLimit = foldr (max . getStmtLimit) 1

allToJVM :: Program -> String -> String
allToJVM (Prog stmts) fname  =
  let (code, map) = runState(runReaderT (stmtsToJVM stmts) 0) Map.empty
  in let lStack = getLimit stmts
  in let lLocals = Map.size map

  in pre fname ++ ".limit stack " ++ show lStack ++"\n.limit locals " ++
    show lLocals ++"\n"++ code ++ post

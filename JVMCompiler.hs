module JVMCompiler (allToJVM) where

import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State

import AbsInstant


pre :: String -> String
pre fname = ".class  public " ++ fname ++ "\n" ++
  ".super java/lang/Object\n" ++
  ".method public <init>()V\n" ++
  "aload_0\n" ++
  "invokespecial java/lang/Object/<init>()V\n" ++
  "return\n" ++
  ".end method\n" ++
  ".method public static main([Ljava/lang/String;)V\n"
post :: String
post = "return \n.end method"

type JVMonad a = State (Map Ident Integer) a

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
printStream = "getstatic java/lang/System/out Ljava/io/PrintStream;\n"
invokeVirtual :: String
invokeVirtual = "invokevirtual java/io/PrintStream/println(I)V\n"

stmtsToJVM :: [Stmt] -> JVMonad (String, Integer)

stmtsToJVM (SAss string aexpr:rest) = do
  (code, depth) <- aexprToJVM aexpr
  map <- get
  when (not (Map.member string map))$ modify (Map.insert string (1 + (toInteger $ Map.size map)))
  map <- get
  let id = fromJust (Map.lookup string map)
  (jvmRest, depth2) <- stmtsToJVM rest
  return (code ++ storeVar id ++"\n" ++ jvmRest, max depth depth2)

stmtsToJVM (SExp aexpr:rest) = do
  (code, depth) <- aexprToJVM aexpr
  (jvmRest, depth2) <-  stmtsToJVM rest
  return (code ++ printStream ++ "swap\n" ++ invokeVirtual ++ jvmRest, max (max depth 2) depth2)

stmtsToJVM ([]) =
  return ("", 0)

aexprToJVM :: Exp -> JVMonad (String, Integer)

aexprToJVM (ExpVar string) =  do
  var <- get
  -- UWAGA sprawdzanie niezdefiniowane zmiennej also w llvm 
  let i = fromJust (Map.lookup string var)
  return  (pushVar i ++ "\n", 1)

aexprToJVM (ExpLit integer) = return (pushConst integer ++ "\n", 1)

aexprToJVM (ExpAdd ex1 ex2) = opToJVM ex1 ex2 "add" True
aexprToJVM (ExpMul ex1 ex2) = opToJVM ex1 ex2 "mul" True

aexprToJVM (ExpSub ex1 ex2) = opToJVM ex1 ex2 "sub" False
aexprToJVM (ExpDiv ex1 ex2) = opToJVM ex1 ex2 "div" False

opToJVM :: Exp -> Exp -> String -> Bool -> JVMonad (String, Integer)
opToJVM ex1 ex2 op comm  = do
  (code1, depth1) <- aexprToJVM ex1
  (code2, depth2) <- aexprToJVM ex2
  if depth2 > depth1 then return (code2 ++ code1 ++ (if not comm then "swap\n" else "") ++ " i" ++ op ++ "\n", depth2)
  else return (code1 ++ code2 ++ " i" ++ op ++"\n", max depth1 (depth2 + 1))


allToJVM :: Program -> String -> String
allToJVM (Prog stmts) fname  =
  let ((code, depth), map) = runState(stmtsToJVM stmts) Map.empty
  in let lLocals = 1 + Map.size map

  in pre fname ++ ".limit stack " ++ show depth ++"\n.limit locals " ++
    show lLocals ++"\n"++ code ++ post

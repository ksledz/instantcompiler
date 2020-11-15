module Main where

import System.Environment ( getArgs )

import LexInstant
import ParInstant
import JVMCompiler
import qualified Data.Text as T
import qualified Data.Maybe
import System.Process

import ErrM

slash :: T.Text
slash = T.pack("/")
ico :: T.Text
ico = T.pack(".ins")

basename :: T.Text -> T.Text
basename f = Data.Maybe.fromJust(T.stripSuffix ico (last $ T.splitOn slash f))

outputName :: String -> String
outputName f = T.unpack(Data.Maybe.fromJust(T.stripSuffix ico (T.pack f))) ++ ".j"

basestring :: String -> String
basestring f = T.unpack(basename(T.pack f))

dirname :: String -> String
dirname f = let (dir, _) = T.breakOnEnd slash  (T.pack f) in case T.unpack(dir) of
    "" ->  "."
    x -> x

main :: IO ()
main = do
  args <- getArgs
  text <- readFile $ head $ args
  case pProgram $ myLexer $ text of
    Bad s -> do putStrLn  (s ++ "\n parse failed \n")
    Ok tree -> do 
        writeFile (outputName $ head $ args) (allToJVM tree (basestring $head $ args))
        callProcess "java" ["-jar", "lib/jasmin.jar", "-d", dirname(head args), outputName $ head $ args]

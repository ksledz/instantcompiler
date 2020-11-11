module Main where
import Text.Megaparsec
import Parser


main :: IO ()
main = do
  --input <- getContents
  parseTest whileParser "2 + 2; papaj = 21 * 37; ziolo = papaj + kremuwka;"

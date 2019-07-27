module Main where

import Control.Monad.Trans.State (evalStateT)
import Text.Megaparsec
import Text.Megaparsec.Char
import GenericParsers
import ESParser
import ESTypes
import Target

main :: IO ()
main = do
  input <- getContents
  return $ length $! input
  case parse (between sc eof parseESInput) "" input of
    Left err -> putStrLn (errorBundlePretty err)
    Right res@(ESData kb queries) -> do
      print res
      evalStateT (resolveQueries queries) kb

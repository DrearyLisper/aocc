module Main where

import qualified AOCC.Grammar as Grammar
import qualified AOCC.Parsing as Parsing

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ head args
    print $ Parsing.parse Grammar.package content
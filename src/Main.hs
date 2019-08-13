module Main where

import Prelude hiding (putStrLn)
import Data.Text.Lazy.IO (putStrLn)

import LLVM.Pretty (ppllvm)

import Lib

main :: IO ()
main = getContents >>= putStrLn . ppllvm . compile

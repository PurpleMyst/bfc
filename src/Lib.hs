{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Lib (compile) where

import Control.Monad (foldM_)
import Data.Word (Word32)
import qualified Data.Map.Strict as M

import LLVM.AST (Operand(..), Name, mkName, Module)
import LLVM.AST.Type (Type(..), ptr, void, i8, i32, i64)
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Constant as C

import LLVM.IRBuilder.Module (buildModule, function, extern)
import LLVM.IRBuilder.Monad (named, emitBlockStart)
import LLVM.IRBuilder.Instruction (ret, alloca, add, sub, gep, call, load, store, condBr, icmp)

-- | Map one end of a bf loop to its other end
createJumpMap :: String -> M.Map Integer Integer
createJumpMap = fst . foldl go (M.empty, []) . filter (\(_, c) -> c == '[' || c == ']') . zip [0..]
  where
    go (m,     s) (i, '[') = (m, i : s)
    go (m, j : s) (i, ']') = ((M.insert i j . M.insert j i) m, s)

-- | Create an integer constant
int :: Integer -> Word32 -> Operand
int value bits = ConstantOperand $ C.Int { C.integerValue = value, C.integerBits = bits }

load' = flip load 0

store' = flip store 0

-- | Call a function without argument attributes
call' f = call f . map (, [])

-- | Load an integer value, modify it using some function, and store it back.
modify f op = load' op >>= f >>= store' op >> return ()

-- | Create a consistent name for a code index
indexToName :: Integer -> Name
indexToName = mkName . show

compile :: String -> Module
compile s = buildModule "main" $
  let
    jumpMap = createJumpMap s
  in
    function "main" [] i32 $ \[] -> do
        emitBlockStart "entry"

        getchar <- extern "getchar" [] i8
        putchar <- extern "putchar" [i8] i32
        memset <- extern "memset" [ptr i8, i8, i64] (ptr i8)

        tape <- alloca i8 (Just $ int 30000 64) 0 `named` "tape"
        call' memset [tape, int 0 8, int 30000 64]

        -- We choose i64 as a type here because code passed through the
        -- LLVM optimizer sign extends this pointer a bunch
        ptr <- alloca i64 Nothing 0 `named` "ptr"
        store' ptr (int 0 64)

        let
          getTapeElement = load' ptr >>= gep tape . return

          getTapeElementValue = getTapeElement >>= load'

          emitLoopCode start end = do
              tapeElementValue <- getTapeElementValue
              isZero <- icmp IP.EQ tapeElementValue (int 0 8)
              condBr isZero end start

          compile' (i, c) = case c of
              '>' -> modify (`add` int 1 64) ptr

              '<' -> modify (`sub` int 1 64) ptr

              '+' -> getTapeElement >>= modify (`add` int 1 8)

              '-' -> getTapeElement >>= modify (`sub` int 1 8)

              '.' -> getTapeElementValue >>= call' putchar . return >> return ()

              ',' -> getTapeElement >>= modify (const $ call getchar [])

              '[' -> do
                let start = i
                let (Just end) = M.lookup start jumpMap
                emitLoopCode (indexToName start) (indexToName end)
                emitBlockStart (indexToName start)

              ']' -> do
                let end = i
                let (Just start) = M.lookup end jumpMap
                emitLoopCode (indexToName start) (indexToName end)
                emitBlockStart (indexToName end)

              _ -> return ()

        mapM_ compile' $ zip [0..] s

        ret $ int 0 32

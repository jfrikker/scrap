{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.Lazy.IO as TIO
import qualified Joe.Prim as Prim
import qualified Joe.LLIR as LLIR
import qualified Joe.LLVM as LLVM
import LLVM.AST (defaultModule)
import qualified LLVM.AST
import LLVM.Pretty (ppllvm)
import qualified System.IO as IO

main :: IO ()
main = IO.withFile "out.ll" IO.WriteMode $ \h -> TIO.hPutStrLn h $ ppllvm mod
  where prog = [LLIR.Function "_4main" [] $ LLIR.Call "_9incrementa3i64" [LLIR.I64Literal 123] LLIR.I64Type,
          LLIR.Function "_3one" [] $ LLIR.I64Literal 1,
          LLIR.Function "_9incrementa3i64" [LLIR.I64Type] $ LLIR.Binary Prim.Add (LLIR.Argument 0 LLIR.I64Type) (LLIR.Call "_3one" [] LLIR.I64Type)
          ]
        mod = defaultModule {
          LLVM.AST.moduleName = "out",
          LLVM.AST.moduleDefinitions = map LLVM.writeFunction prog
          }

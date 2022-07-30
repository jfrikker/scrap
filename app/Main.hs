{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.Lazy.IO as TIO
import qualified LLVM
import LLVM.AST (defaultModule)
import qualified LLVM.AST
import LLVM.Pretty (ppllvm)
import qualified LLIR
import qualified System.IO as IO

main :: IO ()
main = IO.withFile "out.ll" IO.WriteMode $ \h -> TIO.hPutStrLn h $ ppllvm mod
  where prog = [("_4main", LLIR.Function [] $ LLIR.StaticFunctionCall "_9increment" [LLIR.I64Literal 123] LLIR.I64Type),
          ("_3one", LLIR.Function [] $ LLIR.I64Literal 1),
          ("_9increment", LLIR.Function [LLIR.I64Type] $ LLIR.Binary LLIR.Add (LLIR.Argument 0 LLIR.I64Type) (LLIR.StaticFunctionCall "_3one" [] LLIR.I64Type))
          ]
        mod = defaultModule {
          LLVM.AST.moduleName = "out",
          LLVM.AST.moduleDefinitions = LLVM.writeDefinitions prog
          }

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as TIO
import qualified Joe.Prim as Prim
import qualified Joe.LLIR as LLIR
import qualified Joe.LLVM as LLVM
import Joe.Passes.StaticFunctionTemplateElimination (sfte)
import LLVM.AST (defaultModule)
import qualified LLVM.AST
import LLVM.Pretty (ppllvm)
import qualified System.IO as IO

main :: IO ()
main = do
  print src
  print prog
  IO.withFile "out.ll" IO.WriteMode $ \h -> TIO.hPutStrLn h $ ppllvm mod
  where src = Map.fromList [("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "increment") [LLIR.I64Literal 123] LLIR.I64Type),
          ("one", LLIR.Global [] $ LLIR.I64Literal 1),
          ("increment", LLIR.Global [LLIR.I64Type] $ LLIR.Call (LLIR.GlobalReference "flip") [LLIR.Call (LLIR.GlobalReference "add3") [LLIR.I64Literal 0] (LLIR.FunctionType [LLIR.I64Type, LLIR.I64Type] LLIR.I64Type), LLIR.Argument 0 LLIR.I64Type, LLIR.Call (LLIR.GlobalReference "one") [] LLIR.I64Type] LLIR.I64Type),
          ("add3", LLIR.Global [LLIR.I64Type, LLIR.I64Type, LLIR.I64Type] $ LLIR.Binary Prim.Add (LLIR.Binary Prim.Add (LLIR.Argument 0 LLIR.I64Type) (LLIR.Argument 1 LLIR.I64Type)) (LLIR.Argument 2 LLIR.I64Type)),
          ("flip", LLIR.Global [LLIR.FunctionType [LLIR.I64Type, LLIR.I64Type] LLIR.I64Type, LLIR.I64Type, LLIR.I64Type] $ LLIR.Call (LLIR.Argument 0 (LLIR.FunctionType [LLIR.I64Type, LLIR.I64Type] LLIR.I64Type)) [LLIR.Argument 2 LLIR.I64Type, LLIR.Argument 1 LLIR.I64Type] LLIR.I64Type)
          ]
        prog = Map.toList $ passes src
        mod = defaultModule {
          LLVM.AST.moduleName = "out",
          LLVM.AST.moduleDefinitions = map (uncurry LLVM.writeGlobal) prog
          }

passes :: LLIR.Globals -> LLIR.Globals
passes = sfte

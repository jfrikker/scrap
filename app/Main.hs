{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as TIO
import qualified Joe.Prim as Prim
import qualified Joe.LLIR as LLIR
import qualified Joe.LLVM as LLVM
import Joe.Passes.LowerLambdas (lowerLambdas)
import Joe.Passes.ScopeElimination (eliminateScopes)
import Joe.Passes.StaticFunctionTemplateElimination (sfte)
import LLVM.AST (defaultModule)
import qualified LLVM.AST
import LLVM.Pretty (ppllvm)
import qualified System.IO as IO
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  pPrint src
  pPrint prog
  IO.withFile "out.ll" IO.WriteMode $ \h -> TIO.hPutStrLn h $ ppllvm mod
  where src = Map.fromList [("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "increment") [LLIR.I64Literal 123] LLIR.I64Type),
          ("addBoth", addBoth),
          ("weirdAdd", weirdAdd),
          ("main", main)
          ]
        i64 = LLIR.I64Type
        i64Toi64 = LLIR.FunctionType [i64] i64
        addBoth = LLIR.Global [i64Toi64, i64Toi64, i64] $
          LLIR.Binary Prim.Add
            (LLIR.Call (LLIR.LocalReference 0 0 i64Toi64) [LLIR.LocalReference 0 2 i64] i64)
            (LLIR.Call (LLIR.LocalReference 0 1 i64Toi64) [LLIR.LocalReference 0 2 i64] i64)
        weirdAdd = LLIR.Global [i64, i64, i64] $
          LLIR.Scope [
            LLIR.Lambda [i64] $ LLIR.Binary Prim.Add (LLIR.LocalReference 0 0 i64) $ LLIR.LocalReference 2 0 i64,
            LLIR.Lambda [i64] $ LLIR.Binary Prim.Add (LLIR.LocalReference 0 0 i64) $ LLIR.LocalReference 2 1 i64
          ] $ LLIR.Call (LLIR.GlobalReference "addBoth") [
              LLIR.LocalReference 0 0 i64Toi64,
              LLIR.LocalReference 0 1 i64Toi64,
            LLIR.LocalReference 1 2 i64] i64
        main = LLIR.Global [] $
          LLIR.Call (LLIR.GlobalReference "weirdAdd") [LLIR.I64Literal 1, LLIR.I64Literal 2, LLIR.I64Literal 3] i64
        prog = Map.toList $ passes src
        mod = defaultModule {
          LLVM.AST.moduleName = "out",
          LLVM.AST.moduleDefinitions = map (uncurry LLVM.writeGlobal) prog
          }

passes :: LLIR.Globals -> LLIR.Globals
passes = sfte . lowerLambdas . eliminateScopes

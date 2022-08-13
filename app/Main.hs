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
  where src = Map.fromList [("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "increment" i64Toi64) [LLIR.I64Literal 123]),
          ("addBoth", addBoth),
          ("weirdAdd", weirdAdd),
          ("main", main)
          ]
        i64 = LLIR.I64Type
        i64Toi64 = LLIR.FunctionType [i64] i64
        addBoth = LLIR.Global [("f1", i64Toi64), ("f2", i64Toi64), ("i", i64)] $
          LLIR.Binary Prim.Add
            (LLIR.Call (LLIR.LocalReference "f1" i64Toi64) [LLIR.LocalReference "i" i64])
            (LLIR.Call (LLIR.LocalReference "f2" i64Toi64) [LLIR.LocalReference "i" i64])
        weirdAdd = LLIR.Global [("i1", i64), ("i2", i64), ("i3", i64)] $
          LLIR.Scope "l1"
            (LLIR.Lambda [("l1", i64)] $ LLIR.Binary Prim.Add (LLIR.LocalReference "l1" i64) $ LLIR.LocalReference "i1" i64) $
            LLIR.Scope "l2"
              (LLIR.Lambda [("l1", i64)] $ LLIR.Binary Prim.Add (LLIR.LocalReference "l1" i64) $ LLIR.LocalReference "i2" i64
            ) $ 
              LLIR.Call (LLIR.GlobalReference "addBoth" $ LLIR.FunctionType [i64Toi64, i64Toi64, i64] i64) [
                LLIR.LocalReference "l1" i64Toi64,
                LLIR.LocalReference "l2" i64Toi64,
              LLIR.LocalReference "i3" i64]
        main = LLIR.Global [] $
          LLIR.Call (LLIR.GlobalReference "weirdAdd" $ LLIR.FunctionType [i64, i64, i64] i64) [LLIR.I64Literal 1, LLIR.I64Literal 2, LLIR.I64Literal 3]
        prog = Map.toList $ passes src
        mod = defaultModule {
          LLVM.AST.moduleName = "out",
          LLVM.AST.moduleDefinitions = map (uncurry LLVM.writeGlobal) prog
          }

passes :: LLIR.Globals -> LLIR.Globals
passes = sfte . eliminateScopes . lowerLambdas

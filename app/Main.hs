{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as TIO
import qualified Joe.Prim as Prim
import qualified Joe.LLIR as LLIR
import qualified Joe.LLVM as LLVM
import Joe.Parser (moduleParser)
import Joe.Passes.LowerLambdas (lowerLambdas)
import Joe.Passes.ScopeElimination (eliminateScopes)
import Joe.Passes.StaticFunctionTemplateElimination (sfte)
import LLVM.AST (defaultModule)
import qualified LLVM.AST
import LLVM.Pretty (ppllvm)
import qualified System.IO as IO
import Text.Parsec.Text (parseFromFile)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  src <- parseFromFile moduleParser "in.scrap"
  pPrint src
  let Right src' = src
  let prog = Map.toList $ passes src'
  pPrint prog
  let mod = defaultModule {
          LLVM.AST.moduleName = "out",
          LLVM.AST.moduleDefinitions = map (uncurry LLVM.writeGlobal) prog
          }
  IO.withFile "out.ll" IO.WriteMode $ \h -> TIO.hPutStrLn h $ ppllvm mod

passes :: LLIR.Globals -> LLIR.Globals
passes = sfte . eliminateScopes . lowerLambdas

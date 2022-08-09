module StaticFunctionTemplateEliminationSpec (
 spec
) where

import qualified Data.Map as Map
import qualified Joe.LLIR as LLIR
import qualified Joe.Prim as Prim
import Joe.Passes.StaticFunctionTemplateElimination (sfte)
import Test.Hspec

i64 :: LLIR.Type
i64 = LLIR.I64Type

spec :: Spec
spec = do
  describe "ScopeEliminationPass" $ do
    it "inlines a global function" $ do
      let before = Map.fromList [
                     ("add", LLIR.Global [i64, i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.LocalReference 0 1 i64)),
                     ("flip", LLIR.Global [LLIR.FunctionType [i64, i64] i64, i64, i64] $
                       LLIR.Call (LLIR.LocalReference 0 0 $ LLIR.FunctionType [i64, i64] i64) [
                         LLIR.LocalReference 0 2 i64,
                         LLIR.LocalReference 0 1 i64]),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "flip" $ LLIR.FunctionType [i64, i64] i64) [
                         LLIR.GlobalReference "add" $ LLIR.FunctionType [i64, i64] i64,
                         LLIR.I64Literal 1,
                         LLIR.I64Literal 2])]
      let expected = Map.fromList [
                     ("add", LLIR.Global [i64, i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.LocalReference 0 1 i64)),
                     ("_s3addflip", LLIR.Global [i64, i64] $ LLIR.Call (LLIR.GlobalReference "add"$ LLIR.FunctionType [i64, i64] i64) [
                       LLIR.LocalReference 0 1 i64,
                       LLIR.LocalReference 0 0 i64]),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "_s3addflip" $ LLIR.FunctionType [i64, i64] i64) [
                       LLIR.I64Literal 1,
                       LLIR.I64Literal 2])]
      sfte before `shouldBe` expected
    it "inlines two global functions" $ do
      let before = Map.fromList [
                     ("add1", LLIR.Global [i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.I64Literal 1)),
                     ("add2", LLIR.Global [i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.I64Literal 1)),
                     ("weirdAdd", LLIR.Global [LLIR.FunctionType [i64] i64, LLIR.FunctionType [i64] i64, i64] $
                       LLIR.Binary Prim.Add
                         (LLIR.Call (LLIR.LocalReference 0 0 $ LLIR.FunctionType [i64] i64) [
                           LLIR.LocalReference 0 2 i64])
                         (LLIR.Call (LLIR.LocalReference 0 1 $ LLIR.FunctionType [i64] i64) [
                           LLIR.LocalReference 0 2 i64])),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "weirdAdd" $ LLIR.FunctionType [LLIR.FunctionType [i64] i64, LLIR.FunctionType [i64] i64, i64] i64) [
                         LLIR.GlobalReference "add1" $ LLIR.FunctionType [i64] i64,
                         LLIR.GlobalReference "add2" $ LLIR.FunctionType [i64] i64,
                         LLIR.I64Literal 1])]
      let expected = Map.fromList [
                     ("add1", LLIR.Global [i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.I64Literal 1)),
                     ("add2", LLIR.Global [i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.I64Literal 1)),
                     ("_s4add2_s4add1weirdAdd", LLIR.Global [i64] $ 
                       LLIR.Binary Prim.Add
                         (LLIR.Call (LLIR.GlobalReference "add1" $ LLIR.FunctionType [i64] i64) [LLIR.LocalReference 0 0 i64])
                         (LLIR.Call (LLIR.GlobalReference "add2" $ LLIR.FunctionType [i64] i64) [LLIR.LocalReference 0 0 i64])),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "_s4add2_s4add1weirdAdd" $ LLIR.FunctionType [i64] i64) [
                       LLIR.I64Literal 1])]
      sfte before `shouldBe` expected
    it "chooses the same name for two identical template renders" $ do
      let before = Map.fromList [
                     ("add1", LLIR.Global [i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.I64Literal 1)),
                     ("add2", LLIR.Global [i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.I64Literal 1)),
                     ("weirdAdd", LLIR.Global [LLIR.FunctionType [i64] i64, LLIR.FunctionType [i64] i64, i64] $
                       LLIR.Binary Prim.Add
                         (LLIR.Call (LLIR.LocalReference 0 0 $ LLIR.FunctionType [i64] i64) [
                           LLIR.LocalReference 0 2 i64])
                         (LLIR.Call (LLIR.LocalReference 0 1 $ LLIR.FunctionType [i64] i64) [
                           LLIR.LocalReference 0 2 i64])),
                     ("main", LLIR.Global [] $ LLIR.Binary Prim.Add
                       (LLIR.Call (LLIR.GlobalReference "weirdAdd" $ LLIR.FunctionType [LLIR.FunctionType [i64] i64, LLIR.FunctionType [i64] i64, i64] i64) [
                         LLIR.GlobalReference "add1" $ LLIR.FunctionType [i64] i64,
                         LLIR.GlobalReference "add2" $ LLIR.FunctionType [i64] i64,
                         LLIR.I64Literal 1])
                       (LLIR.Call (LLIR.GlobalReference "weirdAdd" $ LLIR.FunctionType [LLIR.FunctionType [i64] i64, LLIR.FunctionType [i64] i64, i64] i64) [
                         LLIR.GlobalReference "add1" $ LLIR.FunctionType [i64] i64,
                         LLIR.GlobalReference "add2" $ LLIR.FunctionType [i64] i64,
                         LLIR.I64Literal 1]))]
      let expected = Map.fromList [
                     ("add1", LLIR.Global [i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.I64Literal 1)),
                     ("add2", LLIR.Global [i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.I64Literal 1)),
                     ("_s4add2_s4add1weirdAdd", LLIR.Global [i64] $ 
                       LLIR.Binary Prim.Add
                         (LLIR.Call (LLIR.GlobalReference "add1" $ LLIR.FunctionType [i64] i64) [LLIR.LocalReference 0 0 i64])
                         (LLIR.Call (LLIR.GlobalReference "add2" $ LLIR.FunctionType [i64] i64) [LLIR.LocalReference 0 0 i64])),
                     ("main", LLIR.Global [] $ LLIR.Binary Prim.Add
                       (LLIR.Call (LLIR.GlobalReference "_s4add2_s4add1weirdAdd" $ LLIR.FunctionType [i64] i64) [
                         LLIR.I64Literal 1])
                       (LLIR.Call (LLIR.GlobalReference "_s4add2_s4add1weirdAdd" $ LLIR.FunctionType [i64] i64) [
                         LLIR.I64Literal 1]))]
      sfte before `shouldBe` expected
    it "doesn't inline a global constant argument" $ do
      let before = Map.fromList [
                     ("one", LLIR.Global [] $ LLIR.I64Literal 1),
                     ("add", LLIR.Global [i64, i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.LocalReference 0 1 i64)),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "add" $ LLIR.FunctionType [i64, i64] i64) [
                       LLIR.GlobalReference "one" $ LLIR.FunctionType [] i64,
                       LLIR.I64Literal 2])]
      sfte before `shouldBe` before

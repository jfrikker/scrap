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
  describe "StaticFunctionTemplateEliminationPass" $ do
    it "inlines a global function" $ do
      let before = Map.fromList [
                     ("add", LLIR.Global [("i1", i64), ("i2", i64)] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference "i1" i64)
                       (LLIR.LocalReference "i2" i64)),
                     ("flip", LLIR.Global [("f", LLIR.FunctionType [i64, i64] i64), ("a1", i64), ("a2", i64)] $
                       LLIR.Call (LLIR.LocalReference "f" $ LLIR.FunctionType [i64, i64] i64) [
                         LLIR.LocalReference "a2" i64,
                         LLIR.LocalReference "a1" i64]),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "flip" $ LLIR.FunctionType [i64, i64] i64) [
                         LLIR.GlobalReference "add" $ LLIR.FunctionType [i64, i64] i64,
                         LLIR.I64Literal 1,
                         LLIR.I64Literal 2])]
      let expected = Map.fromList [
                     ("add", LLIR.Global [("i1", i64), ("i2", i64)] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference "i1" i64)
                       (LLIR.LocalReference "i2" i64)),
                     ("_s3addflip", LLIR.Global [("a1", i64), ("a2", i64)] $ LLIR.Call (LLIR.GlobalReference "add"$ LLIR.FunctionType [i64, i64] i64) [
                       LLIR.LocalReference "a2" i64,
                       LLIR.LocalReference "a1" i64]),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "_s3addflip" $ LLIR.FunctionType [i64, i64] i64) [
                       LLIR.I64Literal 1,
                       LLIR.I64Literal 2])]
      sfte before `shouldBe` expected
    it "inlines two global functions" $ do
      let before = Map.fromList [
                     ("add1", LLIR.Global [("i", i64)] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference "i" i64)
                       (LLIR.I64Literal 1)),
                     ("add2", LLIR.Global [("i", i64)] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference "i" i64)
                       (LLIR.I64Literal 1)),
                     ("weirdAdd", LLIR.Global [("f1", LLIR.FunctionType [i64] i64), ("f2", LLIR.FunctionType [i64] i64), ("i", i64)] $
                       LLIR.Binary Prim.Add
                         (LLIR.Call (LLIR.LocalReference "f1" $ LLIR.FunctionType [i64] i64) [
                           LLIR.LocalReference "i" i64])
                         (LLIR.Call (LLIR.LocalReference "f2" $ LLIR.FunctionType [i64] i64) [
                           LLIR.LocalReference "i" i64])),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "weirdAdd" $ LLIR.FunctionType [LLIR.FunctionType [i64] i64, LLIR.FunctionType [i64] i64, i64] i64) [
                         LLIR.GlobalReference "add1" $ LLIR.FunctionType [i64] i64,
                         LLIR.GlobalReference "add2" $ LLIR.FunctionType [i64] i64,
                         LLIR.I64Literal 1])]
      let expected = Map.fromList [
                     ("add1", LLIR.Global [("i", i64)] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference "i" i64)
                       (LLIR.I64Literal 1)),
                     ("add2", LLIR.Global [("i", i64)] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference "i" i64)
                       (LLIR.I64Literal 1)),
                     ("_s4add2_s4add1weirdAdd", LLIR.Global [("i", i64)] $ 
                       LLIR.Binary Prim.Add
                         (LLIR.Call (LLIR.GlobalReference "add1" $ LLIR.FunctionType [i64] i64) [LLIR.LocalReference "i" i64])
                         (LLIR.Call (LLIR.GlobalReference "add2" $ LLIR.FunctionType [i64] i64) [LLIR.LocalReference "i" i64])),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "_s4add2_s4add1weirdAdd" $ LLIR.FunctionType [i64] i64) [
                       LLIR.I64Literal 1])]
      sfte before `shouldBe` expected
    it "chooses the same name for two identical template renders" $ do
      let before = Map.fromList [
                     ("add1", LLIR.Global [("i", i64)] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference "i" i64)
                       (LLIR.I64Literal 1)),
                     ("add2", LLIR.Global [("i", i64)] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference "i" i64)
                       (LLIR.I64Literal 1)),
                     ("weirdAdd", LLIR.Global [("f1", LLIR.FunctionType [i64] i64), ("f2", LLIR.FunctionType [i64] i64), ("i", i64)] $
                       LLIR.Binary Prim.Add
                         (LLIR.Call (LLIR.LocalReference "f1" $ LLIR.FunctionType [i64] i64) [
                           LLIR.LocalReference "i" i64])
                         (LLIR.Call (LLIR.LocalReference "f2" $ LLIR.FunctionType [i64] i64) [
                           LLIR.LocalReference "i" i64])),
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
                     ("add1", LLIR.Global [("i", i64)] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference "i" i64)
                       (LLIR.I64Literal 1)),
                     ("add2", LLIR.Global [("i", i64)] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference "i" i64)
                       (LLIR.I64Literal 1)),
                     ("_s4add2_s4add1weirdAdd", LLIR.Global [("i", i64)] $ 
                       LLIR.Binary Prim.Add
                         (LLIR.Call (LLIR.GlobalReference "add1" $ LLIR.FunctionType [i64] i64) [LLIR.LocalReference "i" i64])
                         (LLIR.Call (LLIR.GlobalReference "add2" $ LLIR.FunctionType [i64] i64) [LLIR.LocalReference "i" i64])),
                     ("main", LLIR.Global [] $ LLIR.Binary Prim.Add
                       (LLIR.Call (LLIR.GlobalReference "_s4add2_s4add1weirdAdd" $ LLIR.FunctionType [i64] i64) [
                         LLIR.I64Literal 1])
                       (LLIR.Call (LLIR.GlobalReference "_s4add2_s4add1weirdAdd" $ LLIR.FunctionType [i64] i64) [
                         LLIR.I64Literal 1]))]
      sfte before `shouldBe` expected
    it "handles partial function application in arguments" $ do
      let before = Map.fromList [
                     ("add", LLIR.Global [("a1", i64), ("a2", i64)] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference "a1" i64)
                       (LLIR.LocalReference "a2" i64)),
                     ("weirdAdd", LLIR.Global [("i", i64), ("f", LLIR.FunctionType [i64] i64)] $
                       LLIR.Binary Prim.Add
                         (LLIR.LocalReference "i" i64)
                         (LLIR.Call (LLIR.LocalReference "f" $ LLIR.FunctionType [i64] i64) [
                           LLIR.LocalReference "i" i64])),
                     ("main", LLIR.Global [] $
                       (LLIR.Call (LLIR.GlobalReference "weirdAdd" $ LLIR.FunctionType [i64, LLIR.FunctionType [i64] i64] i64) [
                         LLIR.I64Literal 1,
                         (LLIR.Call (LLIR.GlobalReference "add" $ LLIR.FunctionType [i64, i64] i64) [
                           LLIR.I64Literal 2])]))]
      let expected = Map.fromList [
                     ("add", LLIR.Global [("a1", i64), ("a2", i64)] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference "a1" i64)
                       (LLIR.LocalReference "a2" i64)),
                     ("_sp1f3addweirdAdd", LLIR.Global [("i", i64), ("c1", i64)] $ 
                       LLIR.Binary Prim.Add
                         (LLIR.LocalReference "i" i64)
                         (LLIR.Call (LLIR.GlobalReference "add" $ LLIR.FunctionType [i64, i64] i64) [
                           LLIR.LocalReference "c1" i64,
                           LLIR.LocalReference "i" i64])),
                     ("main", LLIR.Global [] $
                       (LLIR.Call (LLIR.GlobalReference "_sp1f3addweirdAdd" $ LLIR.FunctionType [i64, i64] i64) [
                         LLIR.I64Literal 1,
                         LLIR.I64Literal 2]))]
      sfte before `shouldBe` expected
    it "doesn't inline a global constant argument" $ do
      let before = Map.fromList [
                     ("one", LLIR.Global [] $ LLIR.I64Literal 1),
                     ("add", LLIR.Global [("a1", i64), ("a2", i64)] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference "a1" i64)
                       (LLIR.LocalReference "a2" i64)),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "add" $ LLIR.FunctionType [i64, i64] i64) [
                       LLIR.Call (LLIR.GlobalReference "one" $ LLIR.FunctionType [] i64) [],
                       LLIR.I64Literal 2])]
      sfte before `shouldBe` before
    it "doesn't inline a non-partial application" $ do
      let before = Map.fromList [
                     ("add", LLIR.Global [("a1", i64), ("a2", i64)] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference "a1" i64)
                       (LLIR.LocalReference "a2" i64)),
                     ("main", LLIR.Global [] $
                       (LLIR.Call (LLIR.GlobalReference "add" $ LLIR.FunctionType [i64, i64] i64) [
                         LLIR.I64Literal 1,
                         (LLIR.Call (LLIR.GlobalReference "add" $ LLIR.FunctionType [i64, i64] i64) [
                           LLIR.I64Literal 2,
                           LLIR.I64Literal 3])]))]
      sfte before `shouldBe` before

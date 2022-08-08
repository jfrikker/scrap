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
    it "inlines a global constant" $ do
      let before = Map.fromList [
                     ("one", LLIR.Global [] $ LLIR.I64Literal 1),
                     ("add", LLIR.Global [i64, i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.LocalReference 0 1 i64)),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "add") [
                       LLIR.GlobalReference "one",
                       LLIR.I64Literal 2] i64)]
      let expected = Map.fromList [
                     ("one", LLIR.Global [] $ LLIR.I64Literal 1),
                     ("_s3oneadd", LLIR.Global [i64] $ LLIR.Binary Prim.Add
                       (LLIR.GlobalReference "one")
                       (LLIR.LocalReference 0 0 i64)),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "_s3oneadd") [
                       LLIR.I64Literal 2] i64)]
      sfte before `shouldBe` expected
    it "inlines two global constants" $ do
      let before = Map.fromList [
                     ("one", LLIR.Global [] $ LLIR.I64Literal 1),
                     ("two", LLIR.Global [] $ LLIR.I64Literal 2),
                     ("add", LLIR.Global [i64, i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.LocalReference 0 1 i64)),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "add") [
                       LLIR.GlobalReference "one",
                       LLIR.GlobalReference "two"] i64)]
      let expected = Map.fromList [
                     ("one", LLIR.Global [] $ LLIR.I64Literal 1),
                     ("two", LLIR.Global [] $ LLIR.I64Literal 2),
                     ("_s3two_s3oneadd", LLIR.Global [] $ LLIR.Binary Prim.Add
                       (LLIR.GlobalReference "one")
                       (LLIR.GlobalReference "two")),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "_s3two_s3oneadd") [] i64)]
      sfte before `shouldBe` expected
    it "inlines two identical global constants" $ do
      let before = Map.fromList [
                     ("one", LLIR.Global [] $ LLIR.I64Literal 1),
                     ("add", LLIR.Global [i64, i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.LocalReference 0 1 i64)),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "add") [
                       LLIR.GlobalReference "one",
                       LLIR.GlobalReference "one"] i64)]
      let expected = Map.fromList [
                     ("one", LLIR.Global [] $ LLIR.I64Literal 1),
                     ("_s3one_s3oneadd", LLIR.Global [] $ LLIR.Binary Prim.Add
                       (LLIR.GlobalReference "one")
                       (LLIR.GlobalReference "one")),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "_s3one_s3oneadd") [] i64)]
      sfte before `shouldBe` expected
    it "inlines a global function" $ do
      let before = Map.fromList [
                     ("add", LLIR.Global [i64, i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.LocalReference 0 1 i64)),
                     ("flip", LLIR.Global [LLIR.FunctionType [i64, i64] i64, i64, i64] $
                       LLIR.Call (LLIR.LocalReference 0 0 $ LLIR.FunctionType [i64, i64] i64) [
                         LLIR.LocalReference 0 2 i64,
                         LLIR.LocalReference 0 1 i64] i64),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "flip") [
                         LLIR.GlobalReference "add",
                         LLIR.I64Literal 1,
                         LLIR.I64Literal 2] i64)]
      let expected = Map.fromList [
                     ("add", LLIR.Global [i64, i64] $ LLIR.Binary Prim.Add
                       (LLIR.LocalReference 0 0 i64)
                       (LLIR.LocalReference 0 1 i64)),
                     ("_s3addflip", LLIR.Global [i64, i64] $ LLIR.Call (LLIR.GlobalReference "add") [
                       LLIR.LocalReference 0 1 i64,
                       LLIR.LocalReference 0 0 i64] i64),
                     ("main", LLIR.Global [] $ LLIR.Call (LLIR.GlobalReference "_s3addflip") [
                       LLIR.I64Literal 1,
                       LLIR.I64Literal 2] i64)]
      sfte before `shouldBe` expected

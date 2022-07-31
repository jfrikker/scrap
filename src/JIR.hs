module JIR (

) where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import Data.Word (Word8, Word16, Word32, Word64)
import qualified LLIR as LLIR
import qualified Prim as Prim

data Expression = Binary Prim.BinaryOperation Expression Expression |
  Call Expression Expression |
  I64Literal Word64 |
  Reference String

data Definition = ExpressionDefinition Expression |
  FunctionDefinition Type String Expression

data Type = FunctionType Type Type |
  I64Type

dataType :: Expression -> Map String Definition -> Type
dataType (Binary _ a1 _) scope = dataType a1 scope
dataType (Call func arg) scope = unquantifyCall (dataType func scope) $ dataType arg scope
dataType (I64Literal _) _ = I64Type
dataType (Reference name) scope = snd $ Maybe.fromJust $ Map.lookup name scope

llirFunctionName :: String -> [Type] -> String
llirFunctionName baseName args  = "_" ++ (show $ length baseName) ++ baseName ++ argHash args
  where argHash [] = ""
        argHash args = "a" ++ List.intercalate "a" (map (\a -> (let h = typeHash a in (show $ length h) ++ h)) args)

toLLIRExpr :: Expression -> Map String String -> LLIR.Expression
toLLIRExpr (Binary p a1 a2) scope = LLIR.Binary p (toLLIRExpr a1) $ toLLIRExpr a2
toLLIRExpr c@(Call _ _) scope = LLIR.StaticFunctionCall funcName (map dataType args) $ toLLIRType $ dataType c
  where toLLIRExpr' (Call (Reference name) a) = (name, [a])
        toLLIRExpr' (Call f a) = let (name, a') = toLLIRExpr' f in (name, a' ++ [a])
        (name, args) = toLLIRExpr' c
        argTypes = map dataType args
        funcName = llirFunctionName name argTypes

  -- TypeReference String |
  -- UniversalQuantification String Type

-- unquantify :: Type -> (String, Type) -> Type
-- unquantify (FunctionType a r) b = FunctionType (unquantify a b) $ unquantify r b
-- unquantify (TypeReference r) (n, t)
--   | r == n = t
--   | otherwise = TypeReference r
-- unquantify t@(UniversalQuantification n t') b@(n', _)
--   | n == n' = t
--   | otherwise = UniversalQuantification n $ unquantify t' b
-- unquantify t _ = t
-- 
-- quantifiedBinding :: Type -> String -> Type -> Maybe Type
-- quantifiedBinding (FunctionType a r) n (FunctionType a' r') = Monoid.getFirst $ (Monoid.First $ quantifiedBinding a n a') <> (Monoid.First $ quantifiedBinding r n r')
-- quantifiedBinding (TypeReference a) n t
--   | a == n = Just t
--   | otherwise = Nothing
-- quantifiedBinding (UniversalQuantification n t) n' t'
--   | n == n' = Nothing
--   | otherwise = quantifiedBinding t n' t'
-- quantifiedBinding _ _ _ = Nothing
-- 
-- quantifiedArgumentBinding :: Type -> String -> Type -> Maybe Type
-- quantifiedArgumentBinding (FunctionType a _) n t = quantifiedBinding a n t
-- quantifiedArgumentBinding (UniversalQuantification _ t) n t' = quantifiedArgumentBinding t n t'
-- quanitifiedArgumentBinding _ _ _ = Nothing
-- 
-- unquantifyCall :: Type -> Type -> Type
-- unquantifyCall (UniversalQuantification n t) a = Maybe.fromMaybe (UniversalQuantification n uq) binding
--   where uq = unquantifyCall t a
--         binding = quantifiedArgumentBinding uq n a

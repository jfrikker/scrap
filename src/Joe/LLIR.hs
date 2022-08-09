module Joe.LLIR (
  addArg,
  arguments,
  closedOver,
  dataType,
  Expression(..),
  Global(..),
  Globals,
  globalDataType,
  flattenExpressions,
  mangle,
  mangleType,
  mapArguments,
  mapExpressions,
  mapExpressionsM,
  prependLength,
  replaceArg,
  Type(..),
  uniqueBy,
) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Map (Map)
import Data.Functor.Identity (runIdentity)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Joe.Prim as Prim

data Expression = LocalReference Int Int Type |
  Binary Prim.BinaryOperation Expression Expression |
  Call Expression [Expression] |
  GlobalReference String Type |
  I64Literal Word64 |
  Lambda [Type] Expression |
  Scope [Expression] Expression deriving (Eq, Show)

data Type = FunctionType [Type] Type |
  I64Type deriving (Eq, Show)

data Global = Global [Type] Expression deriving (Eq, Show)

dataType :: Expression -> Type
dataType (Binary _ a1 _) = dataType a1
dataType (Call f args)
  | List.null newArgs = res
  | otherwise = FunctionType newArgs res
  where FunctionType typeArgs res = dataType f
        newArgs = List.drop (length args) typeArgs
dataType (GlobalReference _ t) = t
dataType (I64Literal _) = I64Type
dataType (Lambda args body) = FunctionType args $ dataType body
dataType (LocalReference _ _ t) = t
dataType e = error $ "Unable to compute data type " ++ show e

globalDataType :: Global -> Type
globalDataType (Global args body) = FunctionType args $ dataType body

type Globals = Map String Global

arguments :: Expression -> [Expression]
arguments (Binary op a1 a2) = [a1, a2]
arguments (Call f a) = f : a
arguments (Lambda a b) = [b]
arguments (Scope binds body) = body : binds
arguments e = []

mapArguments :: (Expression -> Expression) -> Expression -> Expression
mapArguments f = runIdentity . mapArgumentsM (return . f)

mapArgumentsM :: Monad m => (Expression -> m Expression) -> Expression -> m Expression
mapArgumentsM f (Binary op a1 a2) = do
  a1' <- f a1
  a2' <- f a2
  return $ Binary op a1' a2'
mapArgumentsM f (Call f' a) = do
  f'' <- f f'
  a' <- mapM f a
  return $ Call f'' a'
mapArgumentsM f (Lambda a b) = do
  b' <- f b
  return $ Lambda a b'
mapArgumentsM f (Scope binds body) = do
  binds' <- mapM f binds
  body' <- f body
  return $ Scope binds' body'
mapArgumentsM _ e = return e

mapExpressions :: (Expression -> Expression) -> Expression -> Expression
mapExpressions f = runIdentity . mapExpressionsM (return . f)

mapExpressionsM :: Monad m => (Expression -> m Expression) -> Expression -> m Expression
mapExpressionsM f e = mapArgumentsM (mapExpressionsM f) e >>= f

flattenExpressions :: Expression -> [Expression]
flattenExpressions e = e : List.concatMap flattenExpressions (arguments e)

flattenPaths :: Expression -> [[Expression]]
flattenPaths = inner []
  where inner p e = (e : p) : List.concatMap (inner (e : p)) (arguments e)

replaceArg :: Int -> [Type] -> Expression -> Global -> Global
replaceArg i newArgs rep (Global args body) = Global (argsBefore ++ newArgs ++ argsAfter) $ mapExpressions inner body
  where inner e@(LocalReference 0 n t)
          | n == i = rep
          | n > i = LocalReference 0 (n + length newArgs - 1) t
          | otherwise = e
        inner e = e
        (argsBefore, (_ : argsAfter)) = List.splitAt i args

prependLength :: String -> String
prependLength s = (show $ length s) ++ s

mangle :: String -> [Type] -> String
mangle basename [] = basename
mangle basename args = "_" ++ prependLength basename ++ "a" ++ List.concatMap (prependLength . mangleType) args

mangleType :: Type -> String
mangleType I64Type = "i64"

closedOver :: Expression -> [(Int, Int, Type)]
closedOver = uniqueBy (\(i, j, _) -> (i, j)) . coords 0
  where coords level (LocalReference i j t)
          | i > level = [(i - level, j, t)]
          | otherwise = []
        coords level e@(Scope _ _) = List.concatMap (coords (level + 1)) $ arguments e
        coords level e@(Lambda _ _) = List.concatMap (coords (level + 1)) $ arguments e
        coords level e = List.concatMap (coords level) $ arguments e

uniqueBy :: Ord b => (a -> b) -> [a] -> [a]
uniqueBy f = List.nubBy (\a1 a2 -> (f a1) == (f a2)) . List.sortBy (\a1 a2 -> compare (f a1) (f a2))

addArg :: (Int, Int) -> Expression -> Expression
addArg _ (LocalReference 0 i t) = LocalReference 0 (i + 1) t
addArg (i, j) e@(LocalReference i' j' t)
  | i + 1 == i' && j == j' = LocalReference 0 0 t
  | otherwise = e
addArg _ e = e

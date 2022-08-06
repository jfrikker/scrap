module Joe.LLIR (
  closedOver,
  dataType,
  Expression(..),
  Global(..),
  Globals,
  flattenExpressions,
  mangle,
  mangleType,
  mapExpressions,
  mapExpressionsM,
  prependLength,
  replaceArg,
  Type(..),
) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Map (Map)
import Data.Functor.Identity (runIdentity)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Joe.Prim as Prim

import Debug.Trace (traceShow, traceShowId)

data Expression = LocalReference Int Int Type |
  Binary Prim.BinaryOperation Expression Expression |
  Call Expression [Expression] Type |
  GlobalReference String |
  I64Literal Word64 |
  Lambda [Type] Expression deriving (Show)

data Type = FunctionType [Type] Type |
  I64Type deriving (Show)

data Global = Global [Type] Expression deriving (Show)

dataType :: Expression -> Type
dataType (LocalReference _ _ t) = t
dataType (Binary _ a1 _) = dataType a1
dataType (I64Literal _) = I64Type
dataType (Call _ _ t) = t

type Globals = Map String Global

mapExpressions :: (Expression -> Expression) -> Expression -> Expression
mapExpressions f = runIdentity . mapExpressionsM (return . f)

mapExpressionsM :: Monad m => (Expression -> m Expression) -> Expression -> m Expression
mapExpressionsM f (Binary op a1 a2) = do
  a1' <- mapExpressionsM f a1
  a2' <- mapExpressionsM f a2
  f $ Binary op a1' a2'
mapExpressionsM f (Call f' a t) = do
  f'' <- mapExpressionsM f f'
  a' <- mapM (mapExpressionsM f) a
  f $ Call f'' a' t
mapExpressionsM f (Lambda a b) = do
  b' <- mapExpressionsM f b
  f $ Lambda a b'
mapExpressionsM f e = f e

flattenExpressions :: Expression -> [Expression]
flattenExpressions e@(Binary op a1 a2) = e : flattenExpressions a1 ++ flattenExpressions a2
flattenExpressions e@(Call f args _) = e : flattenExpressions f ++ List.concatMap flattenExpressions args
flattenExpressions e@(Lambda _ body) = e : flattenExpressions body
flattenExpressions e = [e]

replaceArg :: Int -> [Type] -> Expression -> Global -> Global
replaceArg i newArgs rep (Global args body) = Global (argsBefore ++ newArgs ++ argsAfter) $ mapExpressions inner body
  where inner e@(LocalReference 0 n t)
          | n == i = traceShowId rep
          | n > i = traceShow (n, i) $ LocalReference 0 (n + length newArgs - 1) t
          | otherwise = traceShow (n, i) $ e
        inner e = traceShowId e
        (argsBefore, (_ : argsAfter)) = List.splitAt i args

prependLength :: String -> String
prependLength s = (show $ length s) ++ s

mangle :: String -> [Type] -> String
mangle basename [] = basename
mangle basename args = "_" ++ prependLength basename ++ "a" ++ List.concatMap (prependLength . mangleType) args

mangleType :: Type -> String
mangleType I64Type = "i64"

closedOver :: Expression -> [(Int, Int, Type)]
closedOver expr = List.nubBy (\(i1, j1, _) (i2, j2, _) -> (i1, j1) == (i2, j2)) $
  List.sortBy (\(i1, j1, _) (i2, j2, _) -> compare (i1, j1) (i2, j2)) allCoords
  where coords (LocalReference 0 _ _) = Nothing
        coords (LocalReference i j t) = Just (i - 1, j, t)
        coords _ = Nothing
        allCoords = Maybe.catMaybes $ map coords $ flattenExpressions expr

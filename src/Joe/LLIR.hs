module Joe.LLIR (
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
import Data.Map (Map)
import Data.Functor.Identity (runIdentity)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Joe.Prim as Prim

data Expression = Binary Prim.BinaryOperation Expression Expression |
  GlobalReference String |
  I64Literal Word64 |
  Call Expression [Expression] Type |
  Argument Int Type deriving (Show)

data Type = FunctionType [Type] Type |
  I64Type deriving (Show)

data Global = Global [Type] Expression deriving (Show)

dataType :: Expression -> Type
dataType (Argument _ t) = t
dataType (Binary _ a1 _) = dataType a1
dataType (I64Literal _) = I64Type
dataType (Call _ _ t) = t

type Globals = Map String Global

mapExpressions :: (Expression -> Expression) -> Expression -> Expression
mapExpressions f = runIdentity . mapExpressionsM (return . f)

mapExpressionsM :: Monad m => (Expression -> m Expression) -> Expression -> m Expression
mapExpressionsM f (Binary op a1 a2) = do
  a1' <- f a1
  a2' <- f a2
  f $ Binary op a1' a2'
mapExpressionsM f (Call f' a t) = do
  f'' <- f f'
  a' <- mapM f a
  f $ Call f'' a' t
mapExpressionsM f e = f e

flattenExpressions :: Expression -> [Expression]
flattenExpressions e@(Binary op a1 a2) = e : flattenExpressions a1 ++ flattenExpressions a2
flattenExpressions e@(Call f args _) = e : flattenExpressions f ++ List.concatMap flattenExpressions args
flattenExpressions e = [e]

replaceArg :: Int -> [Type] -> Expression -> Global -> Global
replaceArg i newArgs rep (Global args body) = Global (argsBefore ++ newArgs ++ argsAfter) $ mapExpressions inner body
  where inner e@(Argument n t)
          | n == i = rep
          | n > i = Argument (n + length newArgs - 1) t
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

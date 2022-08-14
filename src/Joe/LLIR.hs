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
  inlineBind,
  isFunctionType,
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

data Expression = LocalReference String Type |
  Binary Prim.BinaryOperation Expression Expression |
  Call Expression [Expression] |
  GlobalReference String Type |
  I64Literal Word64 |
  Lambda [(String, Type)] Expression |
  Reference String |
  Scope String Expression Expression deriving (Eq, Show)

data Type = FunctionType [Type] Type |
  I64Type deriving (Eq, Show)

data Global = Global [(String, Type)] Type Expression deriving (Eq, Show)

dataType :: Expression -> Type
dataType (Binary _ a1 _) = dataType a1
dataType (Call f args)
  | List.null newArgs = res
  | otherwise = FunctionType newArgs res
  where FunctionType typeArgs res = dataType f
        newArgs = List.drop (length args) typeArgs
dataType (GlobalReference _ t) = t
dataType (I64Literal _) = I64Type
dataType (Lambda args body) = FunctionType (map snd args) $ dataType body
dataType (LocalReference _ t) = t
dataType e = error $ "Unable to compute data type " ++ show e

isFunctionType :: Type -> Bool
isFunctionType (FunctionType _ _) = True
isFunctionType _ = False

globalDataType :: Global -> Type
globalDataType (Global args res _) = FunctionType (map snd args) res

type Globals = Map String Global

arguments :: Expression -> [Expression]
arguments (Binary op a1 a2) = [a1, a2]
arguments (Call f a) = f : a
arguments (Lambda a b) = [b]
arguments (Scope _ bind body) = [bind, body]
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
mapArgumentsM f (Scope name bind body) = do
  bind' <- f bind
  body' <- f body
  return $ Scope name bind' body'
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

replaceArg :: String -> [(String, Type)] -> Expression -> Global -> Global
replaceArg i newArgs rep (Global args res body) = Global replacedArgs res $ mapExpressions inner body
  where inner e@(LocalReference n t)
          | n == i = rep
          | otherwise = e
        inner e = e
        replacedArgs = replaceWhere (\(n, _) -> n == i) newArgs args

replaceWhere :: (a -> Bool) -> [a] -> [a] -> [a]
replaceWhere _ _ [] = []
replaceWhere f rep (l:ls)
  | f l = rep ++ replaceWhere f rep ls
  | otherwise = (l : replaceWhere f rep ls)

prependLength :: String -> String
prependLength s = (show $ length s) ++ s

mangle :: String -> [Type] -> String
mangle basename [] = basename
mangle basename args = "_" ++ prependLength basename ++ "a" ++ List.concatMap (prependLength . mangleType) args

mangleType :: Type -> String
mangleType I64Type = "i64"

closedOver :: Expression -> [(String, Type)]
closedOver = uniqueBy (\(i, _) -> (i)) . coords
  where coords (LocalReference i t) = [(i, t)]
        coords e@(Scope name _ _) = List.filter (\(name', _) -> name' == name) $ List.concatMap coords $ arguments e
        coords e@(Lambda args _) = List.filter (\(name', _) -> not $ List.elem name' argNames) $ List.concatMap coords $ arguments e
          where argNames = map fst args
        coords e = List.concatMap coords $ arguments e

uniqueBy :: Ord b => (a -> b) -> [a] -> [a]
uniqueBy f = List.nubBy (\a1 a2 -> (f a1) == (f a2)) . List.sortBy (\a1 a2 -> compare (f a1) (f a2))

inlineBind :: (String, Expression) -> Expression -> Expression
inlineBind (name, val) e@(LocalReference name' _)
  | name' == name = val
  | otherwise = e
inlineBind (name, val) e@(Scope name' bind body)
  | name' == name = e
  | otherwise = Scope name' (inlineBind (name, val) bind) (inlineBind (name, val) body)
inlineBind (name, val) e@(Lambda args body)
  | List.elem name argNames = e
  | otherwise = Lambda args $ inlineBind (name, val) body
  where argNames = map fst args
inlineBind b e = mapArguments (inlineBind b) e

addArg :: (String, Type) -> [(String, Type)] -> [(String, Type)]
addArg (baseName, t) existing = (uniqName 1, t) : existing
  where uniqName i
          | List.filter ((== (newName i)) . fst) existing == [] = newName i
          | otherwise = uniqName $ i + 1
          where newName 1 = baseName
                newName n = baseName ++ show n

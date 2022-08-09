module Joe.Passes.LowerLambdas(
  lowerLambdas
) where

import qualified Joe.LLIR as LLIR
import Joe.Passes.Monad (generateName, modifyExpressions, PassM, runPass, upsertGlobal)

lowerLambdas :: LLIR.Globals -> LLIR.Globals
lowerLambdas = promoteLambdas . openLambdas

openLambdas :: LLIR.Globals -> LLIR.Globals
openLambdas = runPass $ modifyExpressions openLambda

openLambda :: LLIR.Expression -> PassM LLIR.Expression
openLambda (LLIR.Lambda args body) = do
  let closed = LLIR.closedOver body
  let newArgs = (map (\(_, _, t) -> t) closed) ++ args
  let shortClosed = map (\(i, j, _) -> (i, j)) closed
  let newBody = foldr (LLIR.mapExpressions . addArg) body shortClosed
  let newCallArgs = map (\(i, j, t) -> LLIR.LocalReference i j t) closed
  return $ LLIR.Call (LLIR.Lambda newArgs newBody) newCallArgs
openLambda e = return e

promoteLambdas :: LLIR.Globals -> LLIR.Globals
promoteLambdas = runPass $ modifyExpressions promoteLambda

promoteLambda :: LLIR.Expression -> PassM LLIR.Expression
promoteLambda e@(LLIR.Lambda args body) = do
  newName <- generateName "_lambda"
  upsertGlobal newName $ LLIR.Global args body
  return $ LLIR.GlobalReference newName $ LLIR.dataType e
promoteLambda e = return e

addArg :: (Int, Int) -> LLIR.Expression -> LLIR.Expression
addArg _ (LLIR.LocalReference 0 i t) = LLIR.LocalReference 0 (i + 1) t
addArg (i, j) e@(LLIR.LocalReference i' j' t)
  | i + 1 == i' && j == j' = LLIR.LocalReference 0 0 t
  | otherwise = e
addArg _ e = e

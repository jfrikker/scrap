module Joe.Passes.LowerLambdas(
  lowerLambdas
) where

import qualified Joe.LLIR as LLIR
import Joe.Passes.Monad (generateName, modifyExpressions, PassM, runPass, upsertGlobal)

lowerLambdas :: LLIR.Globals -> LLIR.Globals
lowerLambdas = runPass $ modifyExpressions handleExpression

handleExpression :: LLIR.Expression -> PassM LLIR.Expression
handleExpression (LLIR.Lambda args body) = do
  let closed = LLIR.closedOver body
  let newArgs = (map (\(_, _, t) -> t) closed) ++ args
  let shortClosed = map (\(i, j, _) -> (i, j)) closed
  let newBody = foldr (LLIR.mapExpressions . addArg) body shortClosed
  newName <- generateName "_lambda"
  upsertGlobal newName $ LLIR.Global newArgs newBody
  let newCallArgs = map (\(i, j, t) -> LLIR.LocalReference i j t) closed
  return $ LLIR.Call (LLIR.GlobalReference newName) newCallArgs $ LLIR.FunctionType args $ LLIR.dataType body
handleExpression e = return e

addArg :: (Int, Int) -> LLIR.Expression -> LLIR.Expression
addArg _ (LLIR.LocalReference 0 i t) = LLIR.LocalReference 0 (i + 1) t
addArg (i, j) e@(LLIR.LocalReference i' j' t)
  | i + 1 == i' && j == j' = LLIR.LocalReference 0 0 t
  | otherwise = e
addArg _ e = e

module Joe.Passes.LowerLambdas(
  lowerLambdas
) where

import qualified Data.List as List
import qualified Joe.LLIR as LLIR
import Joe.Passes.Monad (generateName, modifyExpressions, PassM, runPass, upsertGlobal)

lowerLambdas :: LLIR.Globals -> LLIR.Globals
lowerLambdas = promoteLambdas . openLambdas

openLambdas :: LLIR.Globals -> LLIR.Globals
openLambdas = runPass $ modifyExpressions openLambda

openLambda :: LLIR.Expression -> PassM LLIR.Expression
openLambda e@(LLIR.Lambda args body) = do
  let closed = LLIR.closedOver e
  let newArgs = List.foldr LLIR.addArg args closed
  let newBody = List.foldr LLIR.inlineBind body $ List.map (\((oldName, t), (newName, _)) -> (oldName, LLIR.LocalReference newName t)) $ List.zip closed newArgs
  let newCallArgs = map (\(n, t) -> LLIR.LocalReference n t) closed
  return $ LLIR.Call (LLIR.Lambda newArgs newBody) newCallArgs
openLambda (LLIR.Call (LLIR.Call f innerArgs) args) = return $ LLIR.Call f (innerArgs ++ args)
openLambda e = return e

promoteLambdas :: LLIR.Globals -> LLIR.Globals
promoteLambdas = runPass $ modifyExpressions promoteLambda

promoteLambda :: LLIR.Expression -> PassM LLIR.Expression
promoteLambda e@(LLIR.Lambda args body) = do
  newName <- generateName "_lambda"
  upsertGlobal newName $ LLIR.Global args (LLIR.dataType body) body
  return $ LLIR.GlobalReference newName $ LLIR.dataType e
promoteLambda e = return e

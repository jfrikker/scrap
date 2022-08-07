module Joe.Passes.ScopeElimination (
  eliminateScopes
) where

import Debug.Trace (traceShowId)
import qualified Data.List as List
import qualified Joe.LLIR as LLIR
import Joe.Passes.Monad (generateName, modifyExpressions, PassM, runPass, upsertGlobal)

eliminateScopes :: LLIR.Globals -> LLIR.Globals
eliminateScopes = globalizeScopes . openScopes . lambdaScopes

lambdaScopes :: LLIR.Globals -> LLIR.Globals
lambdaScopes = runPass $ modifyExpressions (return . lambdaScope)

lambdaScope :: LLIR.Expression -> LLIR.Expression
lambdaScope (LLIR.Scope binds body) = LLIR.Scope binds' body'
  where (binds', body') = foldl inner ([], body) binds
        inner (a, b) e@(LLIR.Lambda _ _) = (a ++ [e], b)
        inner (a, b) e = (a ++ [LLIR.Lambda [] (addScopeLayer e)], replaceBind (0, length a) rep body)
          where rep = LLIR.Call (LLIR.LocalReference 0 (length a) (LLIR.FunctionType [] (LLIR.dataType e))) [] (LLIR.dataType e)
lambdaScope e = e

replaceBind :: (Int, Int) -> LLIR.Expression -> LLIR.Expression -> LLIR.Expression
replaceBind (i, j) rep e@(LLIR.LocalReference i' j' _)
  | i == i' && j == j' = rep
  | otherwise = e
replaceBind (i, j) rep (LLIR.Scope binds body) = LLIR.Scope newBinds newBody
  where newBinds = map (replaceBind (i+1, j) (addScopeLayer rep)) binds
        newBody = replaceBind (i+1, j) (addScopeLayer rep) body
replaceBind (i, j) rep (LLIR.Lambda args body) = LLIR.Lambda args $ replaceBind (i+1, j) (addScopeLayer rep) body
replaceBind (i, j) rep e = LLIR.mapArguments (replaceBind (i, j) rep) e

addScopeLayer :: LLIR.Expression -> LLIR.Expression
addScopeLayer = LLIR.mapExpressions inner
  where inner (LLIR.LocalReference i j t) = LLIR.LocalReference (i + 1) j t
        inner e = e

removeScopeLayer :: LLIR.Expression -> LLIR.Expression
removeScopeLayer = LLIR.mapExpressions inner
  where inner (LLIR.LocalReference i j t) = LLIR.LocalReference (i - 1) j t
        inner e = e

openScopes :: LLIR.Globals -> LLIR.Globals
openScopes = runPass $ modifyExpressions (return . openScope)

openScope :: LLIR.Expression -> LLIR.Expression
openScope (LLIR.Scope binds body) = LLIR.Scope (addBindArgs closed binds') body'
  where (binds', body') = foldl (openBindCalls closed) (binds, body) [0..length binds - 1]
        closed = map (closedOver binds) [0..length binds - 1]
openScope e = e

addBindArgs :: [[(Int, Int, LLIR.Type)]] -> [LLIR.Expression] -> [LLIR.Expression]
addBindArgs closedOver = map inner . zip closedOver
  where inner (c, (LLIR.Lambda args body)) = LLIR.Lambda (types ++ args) $ foldr addArg body coords
          where types = map (\(_, _, t) -> t) c
                coords = map (\(i, j, _) -> (i+1, j, 0)) c

openBindCalls :: [[(Int, Int, LLIR.Type)]] -> ([LLIR.Expression], LLIR.Expression) -> Int -> ([LLIR.Expression], LLIR.Expression)
openBindCalls closedOver (binds, body) idx = (map replace binds, replace body)
  where replace = replaceBind (0, idx) rep
        rep = LLIR.Call (LLIR.LocalReference 0 idx $ LLIR.dataType $ binds !! idx) (map (\(i, j, t) -> LLIR.LocalReference i j t) $ closedOver !! idx) $ LLIR.dataType $ binds !! idx
        
closedOver :: [LLIR.Expression] -> Int -> [(Int, Int, LLIR.Type)]
closedOver binds idx = LLIR.uniqueBy (\(i, j, _) -> (i, j)) $
  List.filter (\(i, _, _) -> i > 0) allCoords
  where closedOverHelper seen b@(0, i, _)
          | List.elem i seen = []
          | otherwise = List.concatMap (closedOverHelper (i:seen)) $ LLIR.closedOver $ binds !! i
        closedOverHelper _ b = [b]
        allCoords = List.concatMap (closedOverHelper [idx]) $ LLIR.closedOver $ binds !! idx

addArg :: (Int, Int, Int) -> LLIR.Expression -> LLIR.Expression
addArg (i, j, argLevel) e@(LLIR.LocalReference i' j' t)
  | argLevel == i' = LLIR.LocalReference i' (j' + 1) t
  | i == i' && j == j' = LLIR.LocalReference argLevel 0 t
  | otherwise = e
addArg (i, j, argLevel) e@(LLIR.Scope _ _) = LLIR.mapArguments (addArg (i + 1, j, argLevel + 1)) e
addArg (i, j, argLevel) e@(LLIR.Lambda _ _) = LLIR.mapArguments (addArg (i + 1, j, argLevel + 1)) e
addArg c e = LLIR.mapArguments (addArg c) e

globalizeScopes :: LLIR.Globals -> LLIR.Globals
globalizeScopes = runPass $ modifyExpressions inner
  where inner (LLIR.Scope binds body) = do
          names <- mapM (\_ -> generateName "_lambda") binds
          let withIdx = zip names [0..]
          let binds' = map (\bind -> foldl (\b' (name, idx) -> replaceBind (0, idx) (LLIR.GlobalReference name) b') bind withIdx) binds
          let named = zip names binds'
          mapM_ (\(name, (LLIR.Lambda args body)) -> upsertGlobal name $ LLIR.Global args body) named
          let body' = foldl (\b (name, idx) -> replaceBind (0, idx) (LLIR.GlobalReference name) b) body withIdx
          return $ removeScopeLayer body'
        inner e = return e

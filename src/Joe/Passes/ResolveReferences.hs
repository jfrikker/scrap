module Joe.Passes.ResolveReferences (
  resolveReferences
) where

import Data.Map ((!), Map)
import qualified Data.Map as Map
import qualified Joe.LLIR as LLIR
import Joe.Passes.Monad

import Debug.Trace (traceShowId)

resolveReferences :: LLIR.Globals -> LLIR.Globals
resolveReferences globs = runPass (modifyGlobals (return . resolveGlobalReferences scope)) globs
  where scope = globalRefs globs

resolveGlobalReferences :: Map String LLIR.Expression -> (String, LLIR.Global) -> LLIR.Expression
resolveGlobalReferences scope (_, LLIR.Global args retType body) = resolveExprReferences nested body
  where nested = Map.union (Map.fromList $ map (\(name, t) -> (name, LLIR.LocalReference name t)) args) scope

resolveExprReferences :: Map String LLIR.Expression -> LLIR.Expression -> LLIR.Expression
resolveExprReferences scope (LLIR.Reference name) = scope ! name
resolveExprReferences scope (LLIR.Scope name bind body) = LLIR.Scope name newBind $ resolveExprReferences nested body
  where newBind = resolveExprReferences scope bind
        nested = Map.insert name (LLIR.LocalReference name $ LLIR.dataType newBind) scope
resolveExprReferences scope e = LLIR.mapArguments (resolveExprReferences scope) e

globalRefs :: LLIR.Globals -> Map String LLIR.Expression
globalRefs = Map.mapWithKey inner
  where inner name (LLIR.Global args retType _) = LLIR.GlobalReference name $ LLIR.FunctionType (map snd args) retType

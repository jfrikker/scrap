module Joe.Passes.ScopeElimination (
  eliminateScopes
) where

import Debug.Trace (traceShowId)
import qualified Data.List as List
import qualified Joe.LLIR as LLIR
import Joe.Passes.Monad (generateName, modifyExpressions, PassM, runPass, upsertGlobal)

eliminateScopes :: LLIR.Globals -> LLIR.Globals
eliminateScopes = runPass $ modifyExpressions $ return . eliminateScope

eliminateScope :: LLIR.Expression -> LLIR.Expression
eliminateScope (LLIR.Scope name bind body) = LLIR.inlineBind (name, bind) body
eliminateScope e = e

module Joe.Passes.StaticFunctionTemplateElimination (
  sfte
) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Joe.LLIR as LLIR
import Joe.Passes.Monad (findGlobal, modifyExpressions, PassM, runPass, upsertGlobal)
import Joe.Passes.RemoveUnusedGlobals (rug)

sfte :: LLIR.Globals -> LLIR.Globals
sfte globs = rug $ runPass (modifyExpressions handleExpression) globs

handleExpression :: LLIR.Expression -> PassM LLIR.Expression
handleExpression e@(LLIR.Call (LLIR.GlobalReference name) args t) = do
    template <- findGlobal name
    let (newArgs, rendered, renderedName, changed) = foldl (\(newA, newG, newN, c) a -> let (a', g', n', c') = maybeExpandArg (a, length newA, newG, newN) in (newA ++ a', g', n', c || c')) ([], template, name, False) args
    if changed then do
      upsertGlobal renderedName rendered
      return $ LLIR.Call (LLIR.GlobalReference renderedName) newArgs t
    else return e
handleExpression e = return e

maybeExpandArg :: (LLIR.Expression, Int, LLIR.Global, String) -> ([LLIR.Expression], LLIR.Global, String, Bool)
maybeExpandArg (a@(LLIR.GlobalReference f), i, glob, name) = ([], LLIR.replaceArg i [] a glob, "_s" ++ LLIR.prependLength f ++ name, True)
maybeExpandArg (a@(LLIR.Call f@(LLIR.GlobalReference fName) args t@(LLIR.FunctionType _ _)), i, glob, name) = (args, glob', newName, True)
  where argTypes = map LLIR.dataType args
        rep = LLIR.Call f (map (\(idx, a) -> LLIR.LocalReference 0 (i + idx) (LLIR.dataType a)) $ List.zip [0..] args) t
        glob' = LLIR.replaceArg i argTypes rep glob
        newName = "_sp" ++ (show $ length args) ++ "f" ++ LLIR.prependLength fName ++ name
maybeExpandArg (a, _, glob, name) = ([a], glob, name, False)

module Joe.Passes.StaticFunctionTemplateElimination (
  sfte
) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Joe.LLIR as LLIR
import Joe.Passes.Monad (findGlobal, modifyGlobals, PassM, runPass, upsertGlobal)
import Joe.Passes.RemoveUnusedGlobals (rug)

sfte :: LLIR.Globals -> LLIR.Globals
sfte globs = rug $ runPass (modifyGlobals handleGlobal) globs

handleGlobal :: (String, LLIR.Global) -> PassM LLIR.Expression
handleGlobal (_, (LLIR.Global _ body)) = LLIR.mapExpressionsM handleExpression body

handleExpression :: LLIR.Expression -> PassM LLIR.Expression
handleExpression e@(LLIR.Call (LLIR.GlobalReference name) args t)
  | List.null staticArgs = return e
  | otherwise = do
    template <- findGlobal name
    let rendered = foldr (\i -> LLIR.replaceArg i (args !! i)) template staticArgs
    let renderedName = mangle name args
    upsertGlobal renderedName rendered
    return $ LLIR.Call (LLIR.GlobalReference renderedName) (filter (\a -> not $ isStaticFunc a) args) t
  where isStaticFunc (LLIR.GlobalReference _) = True
        isStaticFunc otherwise = False
        staticArgs = List.findIndices isStaticFunc args
handleExpression e = return e

mangle :: String -> [LLIR.Expression] -> String
mangle name args = "_s" ++ (show $ length argNames) ++ "_" ++ LLIR.prependLength name ++ List.concatMap LLIR.prependLength argNames
  where argNames = Maybe.catMaybes $ map argName args
        argName (LLIR.GlobalReference name) = Just name
        argName otherwise = Nothing

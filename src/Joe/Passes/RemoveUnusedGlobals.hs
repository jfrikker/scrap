module Joe.Passes.RemoveUnusedGlobals (
  rug
) where

import Control.Monad (forM_)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Joe.LLIR as LLIR
import Joe.Passes.Monad (allGlobals, removeGlobal, PassM, runPass)

rug :: LLIR.Globals -> LLIR.Globals
rug = runPass $ do
  globs <- allGlobals
  let refs = Set.fromList $ concatMap (\(_, LLIR.Global _ body) -> Maybe.catMaybes $ map referenced $ LLIR.flattenExpressions body) globs
  let names = Set.fromList $ map fst globs
  let toRemove = Set.difference names refs
  forM_ toRemove removeGlobal

referenced :: LLIR.Expression -> Maybe String
referenced (LLIR.GlobalReference name) = Just name
referenced _ = Nothing

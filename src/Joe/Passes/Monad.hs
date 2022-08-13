{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Joe.Passes.Monad (
  allGlobals,
  findGlobal,
  generateName,
  modifyExpressions,
  modifyGlobals,
  PassM,
  removeGlobal,
  runPass,
  upsertGlobal
) where

import Control.Lens ((^.), (%%=), (.=), at, use, uses)
import Control.Lens.TH
import Control.Monad (forM)
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Joe.LLIR as LLIR

data PassState = PassState {
  _name_seq :: Int,
  _globals :: LLIR.Globals
}

makeLenses ''PassState

newtype PassM a = PassM (State PassState a) deriving (Applicative, Functor, Monad)

runPass :: PassM () -> LLIR.Globals -> LLIR.Globals
runPass (PassM s) g = res ^. globals
  where res = (State.execState s PassState {
    _name_seq = 1,
    _globals = g
    })

allGlobals :: PassM [(String, LLIR.Global)]
allGlobals = PassM $ uses globals Map.toList

generateName :: String -> PassM String
generateName base = PassM $ name_seq %%= \s -> ("_" ++ show s ++ "_" ++ base, s + 1)

findGlobal :: String -> PassM LLIR.Global
findGlobal name = PassM $ uses (globals.at name) Maybe.fromJust

upsertGlobal :: String -> LLIR.Global -> PassM ()
upsertGlobal name glob = PassM $ globals.at name .= Just glob

removeGlobal :: String -> PassM ()
removeGlobal name = PassM $ globals.at name .= Nothing

modifyGlobals :: ((String, LLIR.Global) -> PassM LLIR.Expression) -> PassM ()
modifyGlobals f = do
  gbls <- PassM $ use globals
  iterateUntilM Map.null forEachGlobal' gbls
  return ()
  where forEachGlobal' gbls = do
          before <- PassM $ use globals
          forM (Map.toList gbls) modifyGlobal
          after <- PassM $ use $ globals
          return $ Map.difference (Map.difference after before) gbls
        modifyGlobal g@(name, LLIR.Global args ret _) = do
          newBody <- f g
          PassM $ globals.at name .= (Just $ LLIR.Global args ret newBody)
    
modifyExpressions :: (LLIR.Expression -> PassM LLIR.Expression) -> PassM ()
modifyExpressions f = modifyGlobals inner
  where inner (_, LLIR.Global _ _ body) = LLIR.mapExpressionsM f body

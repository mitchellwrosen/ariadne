module Ariadne.Types where

import Language.Haskell.Names
import Language.Haskell.Exts.Annotated
import Distribution.HaskellSuite.Packages
import qualified Distribution.ModuleName as Cabal
import qualified Data.Map as Map

data NameLevel = TypeLevel | ValueLevel
  deriving (Ord, Eq, Show, Enum, Bounded)

-- | Global name index records the correspondence between global names and
-- their definition sites. Not every available global name is necessarily
-- in the index.
type GlobalNameIndex = Map.Map (OrigName, NameLevel) SrcLoc

-- | Data about a module for which we have source code
data ModuleData = ModuleData
  { moduleSource :: Module SrcSpan
  , moduleSymbols :: Symbols
  }

-- | A module collection roughly corresponds to a single Cabal package,
-- although it doesn't have to be cabalized.
--
-- The important thing is that all modules in the collection share the
-- same set of package dependencies (including their versions), and
-- recursive modules have to be in the same collection.
data ModuleCollection = ModuleCollection
  { collectionDeps :: Packages
  , collectionModuleData :: Map.Map Cabal.ModuleName ModuleData
  }

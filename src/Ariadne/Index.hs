{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Ariadne.Index where

import Language.Haskell.Names
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import Language.Haskell.Exts.Annotated
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map

import Ariadne.Types
import qualified Ariadne.SrcMap as SrcMap

data Origin
  = LocKnown SrcLoc
  | LocUnknown ModuleNameS
  | ResolveError String
  deriving Show

mkSrcMap
  :: Foldable a
  => GlobalNameIndex
  -> a (Scoped SrcSpan)
  -> SrcMap.SrcMap Origin
mkSrcMap gIndex =
  foldMap $ \(Scoped nameInfo span) ->
    case nameInfo of
      LocalValue bindingLoc ->
        SrcMap.singleton span (LocKnown bindingLoc)
      GlobalValue (sv_origName -> orig) ->
        SrcMap.singleton span $ findGlobal ValueLevel orig
      GlobalType  (st_origName -> orig) ->
        SrcMap.singleton span $ findGlobal TypeLevel  orig
      ScopeError er ->
        SrcMap.singleton span $ ResolveError $ ppError er
      _ -> SrcMap.empty
  where
    findGlobal level orig =
      maybe
        (LocUnknown $ gModule . origGName $ orig)
        LocKnown
        (Map.lookup (orig, level) gIndex)

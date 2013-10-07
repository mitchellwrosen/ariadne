{-# LANGUAGE LambdaCase, TupleSections, TypeFamilies #-}

module Ariadne.GlobalNameIndex (mkGlobalNameIndex) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Language.Haskell.Names (GName(..), OrigName(..))
import Language.Haskell.Names.SyntaxUtils (getModuleName, nameToString, splitDeclHead)
import Language.Haskell.Names.GetBound (getBound)
import Language.Haskell.Exts.Annotated

import qualified Data.Map as Map
import qualified Language.Haskell.Names.GlobalSymbolTable as Global

import Ariadne.Types

mkGlobalNameIndex :: Global.Table -> Module SrcLoc -> GlobalNameIndex
mkGlobalNameIndex tbl mod =
  let
    Module _ _ _ _ ds = mod
    ModuleName _ modname = getModuleName mod
    names = concatMap (indexDecl tbl) ds
  in
    Map.fromList
      [ ((OrigName Nothing (GName modname (nameToString n)), level), ann n) | (n, level) <- names ]

indexDecl :: Global.Table -> Decl SrcLoc -> [(Name SrcLoc, NameLevel)]
indexDecl _ (TypeDecl _ dh _)    = [(hname dh, TypeLevel)]
indexDecl _ (TypeFamDecl _ dh _) = [(hname dh, TypeLevel)]
indexDecl _ (DataDecl _ _ _ dh qualConDecls _) =
    ((hname dh, TypeLevel) :) . map (, ValueLevel) $
        conDecl <$> qualConDecls >>= \case
            ConDecl _ n _        -> [n]
            InfixConDecl _ _ n _ -> [n]
            RecDecl _ n fields   -> n : [f | FieldDecl _ fNames _ <- fields, f <- fNames]
indexDecl tbl (GDataDecl _ dataOrNew _ dh _ gadtDecls _) =
    -- As of 1.14.0, HSE doesn't support GADT records.
    -- When it does, this code should be rewritten similarly to the
    -- DataDecl case.
    -- (Also keep in mind that GHC doesn't create selectors for fields
    -- with existential type variables.)
    (hname dh, TypeLevel) :
        [ (cn, ValueLevel)
        | GadtDecl _ cn _ <- gadtDecls
        ]
indexDecl tbl d@(ClassDecl _ _ dh _ mds) =
    (hname dh, TypeLevel) :
    let
        ms = getBound tbl d
        cdecls = fromMaybe [] mds
    in
        (hname dh, TypeLevel) :
        [ (hname dh, TypeLevel) | ClsTyFam   _   dh _ <- cdecls ] ++
        [ (hname dh, TypeLevel) | ClsDataFam _ _ dh _ <- cdecls ] ++
        [ (mn, ValueLevel) | mn <- ms ]
indexDecl tbl (FunBind _ ms) = map (, ValueLevel) $ getBound tbl ms
indexDecl tbl (PatBind _ p _ _ _) = map (, ValueLevel) $ getBound tbl p
indexDecl _ (ForImp _ _ _ _ fn _) = [(fn, ValueLevel)]
indexDecl _ _ = []

conDecl :: QualConDecl a -> ConDecl a
conDecl (QualConDecl _ _ _ c) = c

hname = fst . splitDeclHead

{-# LANGUAGE TupleSections, TypeFamilies #-}
module Ariadne.GlobalNameIndex (mkGlobalNameIndex) where

import Language.Haskell.Names
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import Language.Haskell.Names.SyntaxUtils
import Language.Haskell.Names.GetBound
import Language.Haskell.Exts.Annotated
import Distribution.HaskellSuite.Modules
import qualified Data.Map as Map
import Data.Maybe

import Ariadne.Types

mkGlobalNameIndex
  :: Global.Table -> Module SrcLoc -> GlobalNameIndex
mkGlobalNameIndex tbl mod =
  let
    Module _ _ _ _ ds = mod
    ModuleName _ modname = getModuleName mod

    names = concatMap (indexDecl tbl) ds

  in
    Map.fromList
      [ ((OrigName Nothing (GName modname (nameToString n)), level), ann n)
      | (n, level) <- names
      ]

indexDecl :: Global.Table -> Decl SrcLoc -> [(Name SrcLoc, NameLevel)]
indexDecl tbl d =
  case d of
    TypeDecl _ dh _ -> [(hname dh, TypeLevel)]
    TypeFamDecl _ dh _ -> [(hname dh, TypeLevel)]

    DataDecl _ _ _ dh qualConDecls _ ->
      ((hname dh, TypeLevel) :) . map (, ValueLevel) $ do -- list monad

      QualConDecl _ _ _ conDecl <- qualConDecls
      case conDecl of
        ConDecl _ n _ -> return n
        InfixConDecl _ _ n _ -> return n
        RecDecl _ n fields ->
          n :
          [f | FieldDecl _ fNames _ <- fields, f <- fNames]

    GDataDecl _ dataOrNew _ dh _ gadtDecls _ ->
      -- As of 1.14.0, HSE doesn't support GADT records.
      -- When it does, this code should be rewritten similarly to the
      -- DataDecl case.
      -- (Also keep in mind that GHC doesn't create selectors for fields
      -- with existential type variables.)
          (hname dh, TypeLevel) :
        [ (cn, ValueLevel)
        | GadtDecl _ cn _ <- gadtDecls
        ]

    ClassDecl _ _ dh _ mds ->
      (hname dh, TypeLevel) :
      let
        ms = getBound tbl d
        cdecls = fromMaybe [] mds
      in
          (hname dh, TypeLevel) :
        [ (hname dh, TypeLevel) | ClsTyFam   _   dh _ <- cdecls ] ++
        [ (hname dh, TypeLevel) | ClsDataFam _ _ dh _ <- cdecls ] ++
        [ (mn, ValueLevel) | mn <- ms ]

    FunBind _ ms -> map (, ValueLevel) $ getBound tbl ms

    PatBind _ p _ _ _ -> map (, ValueLevel) $ getBound tbl p

    ForImp _ _ _ _ fn _ -> [(fn, ValueLevel)]

    _ -> []
  where
    hname = fst . splitDeclHead

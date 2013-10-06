{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
module Main where

import Ariadne.GlobalNameIndex
import Ariadne.Index
import qualified Ariadne.SrcMap as SrcMap

import Language.Haskell.Names
import Language.Haskell.Names.Interfaces
import Language.Haskell.Names.SyntaxUtils
import Language.Haskell.Names.Imports
import Language.Haskell.Exts.Annotated
import Distribution.HaskellSuite.Packages

import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import Control.Exception
import Text.Printf

import Data.BERT
import Network.BERT.Server
import Network.BERT.Transport
import qualified Data.ByteString.Lazy.UTF8 as UTF8

-- these should probably come from the Cabal file
defaultLang = Haskell2010
defaultExts = []

work :: String -> Int -> Int -> IO (Maybe Origin)
work mod line col = handleExceptions $ do
  parseResult <-
    parseFileWithMode
      defaultParseMode { parseFilename = mod }
      mod

  case parseResult of
    ParseFailed loc msg ->
      return $ Just $ ResolveError $ printf "%s: %s" (prettyPrint loc) msg

    ParseOk parsed -> do

      let pkgs = []
      (resolved, impTbl) <-
        flip evalNamesModuleT pkgs $ do
          -- computeInterfaces lang exts mod
          let extSet = moduleExtensions defaultLang defaultExts parsed
          (,) <$>
            (annotateModule defaultLang defaultExts parsed) <*>
            (fmap snd $ processImports extSet $ getImports parsed)

      let
        gIndex = mkGlobalNameIndex impTbl (getPointLoc <$> parsed)
        srcMap = mkSrcMap gIndex (fmap srcInfoSpan <$> resolved)

      return $ SrcMap.lookup noLoc { srcLine = line, srcColumn = col } srcMap
  where
    handleExceptions a =
      try (a >>= evaluate) >>= either (\e -> return $ Just $ ResolveError $ show (e::SomeException)) return

main = do
  t <- fromHostPort "" 39014
  serve t dispatch
  where
    -- dispatch _ _ args = do print args; return $ Success $ NilTerm
    dispatch "ariadne" "find" [BinaryTerm file, IntTerm line, IntTerm col] =
      work (UTF8.toString file) line col >>= \result -> return . Success $
        case result of
          Nothing -> TupleTerm [AtomTerm "no_name"]
          Just (LocKnown (SrcLoc file' line' col')) ->
            TupleTerm
              [ AtomTerm "loc_known"
              , BinaryTerm (UTF8.fromString file')
              , IntTerm line'
              , IntTerm col'
              ]
          Just (LocUnknown modName) ->
            TupleTerm
              [ AtomTerm "loc_unknown"
              , BinaryTerm (UTF8.fromString modName)
              ]
          Just (ResolveError er) ->
            TupleTerm
              [ AtomTerm "error"
              , BinaryTerm (UTF8.fromString er)
              ]
    dispatch _ _ _ = return NoSuchFunction

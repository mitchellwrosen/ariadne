{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Ariadne.GlobalNameIndex
import Ariadne.Index
import qualified Ariadne.SrcMap as SrcMap

import Language.Haskell.Names (annotateModule)
import Language.Haskell.Names.Interfaces (evalNamesModuleT)
import Language.Haskell.Names.SyntaxUtils (getImports, moduleExtensions)
import Language.Haskell.Names.Imports (processImports)
import Language.Haskell.Exts.Annotated

import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Exception (SomeException, evaluate, try)
import Text.Printf (printf)

import Data.BERT (Term(..))
import Network.BERT.Server (DispatchResult(..), serve)
import Network.BERT.Transport (fromHostPort)
import qualified Data.ByteString.Lazy.UTF8 as UTF8

-- these should probably come from the Cabal file
defaultLang = Haskell2010
defaultExts = []

main = fromHostPort "" 39014 >>= flip serve dispatch
  where
    -- dispatch _ _ args = do print args; return $ Success $ NilTerm
    dispatch :: String -> String -> [Term] -> IO DispatchResult
    dispatch "ariadne" "find" [BinaryTerm file, IntTerm line, IntTerm col] =
        toResult <$> work (UTF8.toString file) line col
    dispatch _ _ _ = return NoSuchFunction

    toResult :: Maybe Origin -> DispatchResult
    toResult = Success . TupleTerm . toTerms

    toTerms :: Maybe Origin -> [Term]
    toTerms Nothing = [ AtomTerm "no_name" ]
    toTerms (Just (LocKnown (SrcLoc file' line' col'))) =
        [ AtomTerm "loc_known"
        , BinaryTerm (UTF8.fromString file')
        , IntTerm line'
        , IntTerm col'
        ]
    toTerms (Just (LocUnknown modName)) =
        [ AtomTerm "loc_unknown"
        , BinaryTerm (UTF8.fromString modName)
        ]
    toTerms (Just (ResolveError er)) =
        [ AtomTerm "error"
        , BinaryTerm (UTF8.fromString er)
        ]

work :: FilePath -> Int -> Int -> IO (Maybe Origin)
work filename line col = handleExceptions $ do
    parseFileWithMode parseMode filename >>= \case
        ParseFailed loc msg -> return $ Just $ ResolveError $ printf "%s: %s" (prettyPrint loc) msg
        ParseOk parsed -> do
            (resolved, impTbl) <- flip evalNamesModuleT [] $ do
                -- computeInterfaces lang exts mod
                let extSet = moduleExtensions defaultLang defaultExts parsed
                liftA2 (,)
                  (annotateModule defaultLang defaultExts parsed)
                  (fmap snd $ processImports extSet $ getImports parsed)

            let srcMap = getSrcMap parsed impTbl resolved
            return $ SrcMap.lookup noLoc { srcLine = line, srcColumn = col } srcMap
  where
    handleExceptions :: IO (Maybe Origin) -> IO (Maybe Origin)
    handleExceptions a = try (a >>= evaluate) >>= handleExceptions'
      where
        handleExceptions' :: Either SomeException (Maybe Origin) -> IO (Maybe Origin)
        handleExceptions' = return . either (Just . ResolveError . show) id

    parseMode :: ParseMode
    parseMode = defaultParseMode { parseFilename = filename }

    getSrcMap parsed impTbl resolved = mkSrcMap gIndex (fmap srcInfoSpan <$> resolved)
      where
        gIndex = mkGlobalNameIndex impTbl (getPointLoc <$> parsed)

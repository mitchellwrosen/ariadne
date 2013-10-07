-- | A map indexed by 'SrcSpan' and addressable by 'SrcLoc'.
--
-- Notes:
--
-- * when some of the inserted intervals are overlapping, the behaviour is
-- undefined
{-# LANGUAGE ViewPatterns #-}

module Ariadne.SrcMap
  ( SrcMap
  , empty
  , insert
  , lookup
  , singleton
  , union
  ) where

import Control.Monad (guard)
import Prelude hiding (lookup)
import Language.Haskell.Exts.SrcLoc
import Data.Monoid
import qualified Data.Map as Map

newtype SrcMap a = SrcMap
  { unSrcMap :: Map.Map {- end -} SrcLoc ({- start -} SrcLoc, a)
  } deriving Show

instance Monoid (SrcMap a) where
  mempty  = empty
  mappend = union

insert :: SrcSpan -> a -> SrcMap a -> SrcMap a
insert src_span value = SrcMap . Map.insert (srcSpanEnd' src_span) (srcSpanStart' src_span, value) . unSrcMap

-- | Get the start SrcLoc from a SrcSpan.
srcSpanStart' :: SrcSpan -> SrcLoc
srcSpanStart' (SrcSpan filename start_line start_col _ _) = SrcLoc filename start_line start_col

-- | Get the end SrcLoc from a SrcSpan.
srcSpanEnd' :: SrcSpan -> SrcLoc
srcSpanEnd' (SrcSpan filename _ _ end_line end_col) = SrcLoc filename end_line end_col

lookup :: SrcLoc -> SrcMap a -> Maybe a
lookup loc (SrcMap src_map) = do
  let (_, greater) = Map.split loc src_map
  guard $ Map.null greater
  let (_, (start, value)) = Map.findMin greater
  guard $ start > loc
  return value

union :: SrcMap a -> SrcMap a -> SrcMap a
union (SrcMap a) (SrcMap b) = SrcMap $ a `Map.union` b

empty :: SrcMap a
empty = SrcMap Map.empty

singleton :: SrcSpan -> a -> SrcMap a
singleton src_span value = insert src_span value empty

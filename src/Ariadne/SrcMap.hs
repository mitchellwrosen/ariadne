-- | A map indexed by 'SrcSpan' and addressable by 'SrcLoc'.
--
-- Notes:
--
-- * the 'srcFilename' component is ignored
--
-- * when some of the inserted intervals are overlapping, the behaviour is
-- undefined
{-# LANGUAGE ViewPatterns #-}
module Ariadne.SrcMap
  ( SrcMap
  , insert
  , lookup
  , union
  , empty
  , singleton
  ) where

import Prelude hiding (lookup)
import Language.Haskell.Exts.SrcLoc hiding (Loc(..))
import qualified Data.Map as Map
import Data.Monoid

-- | @Loc line column@
data Loc = Loc !Int !Int
  deriving (Eq, Ord, Show)

newtype SrcMap a =
  SrcMap
  { unSrcMap :: Map.Map {- end -} Loc ({- start -} Loc, a)
  } deriving Show

instance Monoid (SrcMap a) where
  mempty = empty
  mappend = union

spanStart, spanEnd :: SrcSpan -> Loc
spanStart = uncurry Loc . srcSpanStart
spanEnd = uncurry Loc . srcSpanEnd

fromSrcLoc :: SrcLoc -> Loc
fromSrcLoc SrcLoc { srcLine = line, srcColumn = col } = Loc line col

insert :: SrcSpan -> a -> SrcMap a -> SrcMap a
insert span value (SrcMap m) =
  SrcMap $ Map.insert (spanEnd span) (spanStart span, value) m

lookup :: SrcLoc -> SrcMap a -> Maybe a
lookup (fromSrcLoc -> loc) (SrcMap map) =
  case Map.split loc map of
    (_less, greater)
      | Map.null greater -> Nothing
      | otherwise ->
        case Map.findMin greater of
         (_, (start, value))
          | start <= loc -> Just value
          | otherwise -> Nothing

union :: SrcMap a -> SrcMap a -> SrcMap a
union (SrcMap a) (SrcMap b) = SrcMap (a `Map.union` b)

empty :: SrcMap a
empty = SrcMap Map.empty

singleton :: SrcSpan -> a -> SrcMap a
singleton span value = insert span value empty

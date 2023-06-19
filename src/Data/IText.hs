module Data.IText
  ( IText,
    ITextPool,
    emptyPool,
    intern,
  ) where

import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (lookup, size, insert, empty)
import Data.Hashable (Hashable(hashWithSalt))
import GHC.Records (HasField(getField))

-- | Interned text with O(1) comparison and hashing.
data IText = IText Int Text

instance Show IText where
  showsPrec p (IText _ s) = showsPrec p s

instance HasField "id" IText Int where
  getField (IText i _) = i

instance HasField "str" IText Text where
  getField (IText _ str) = str

instance Eq IText where
  a == b = a.id == b.id

instance Ord IText where
  compare a b = compare a.id b.id

instance Hashable IText where
  hashWithSalt salt a = hashWithSalt salt a.id

-- | Text intern pool.
--
-- Caveat 1: do not reuse a pool that was updated.
-- We could enforce this with @LinearTypes@ as follows:
--
-- @
-- intern :: ITextPool ⊸ (Unrestricted IText, ITextPool)
-- @
--
-- Caveat 2: do not mix interned strings from different pools.
-- We could enforce this with @RankNTypes@ as follows:
--
-- @
-- withPool :: (forall p. ITextPool p -> a) -> a
-- intern :: ITextPool p -> (IText p, ITextPool p)
-- @
--
-- Not taking this into account may lead to different strings getting assigned
-- the same identifier. The only reason we do not use @LinearTypes@ and
-- @RankNTypes@ to ensure safe usage is that we would need to use a linear
-- monad with a type parameter for parsing, and @happy@ would not be happy
-- about it.
newtype ITextPool = ITextPool (HashMap Text IText)

emptyPool :: ITextPool
emptyPool = ITextPool HashMap.empty

intern :: Text -> ITextPool -> (IText, ITextPool)
intern str pool@(ITextPool m) =
  case HashMap.lookup str m of
    Just istr -> (istr, pool)
    Nothing ->
      let istr = IText (HashMap.size m + 1) str
          pool' = ITextPool (HashMap.insert str istr m)
      in (istr, pool')
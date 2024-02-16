module Tuya.Orphans where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import NoThunks.Class

instance (NoThunks v) => NoThunks (HashMap k v) where
  showTypeOf _ = "HashMap"
  wNoThunks ctx = noThunksInValues ctx . HM.elems
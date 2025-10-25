module Competences.Frontend.Component.Editor.Editable
  ( Editable(..)
  , editable
  , withModify
  , withDelete
  , withReorder
  )
where

import Competences.Command (Command, ModifyCommand)
import Competences.Document (Document, UserId)
import Competences.Frontend.Component.Editor.Types (ReorderAction)
import GHC.Generics (Generic)

-- | Defines an editable collection of objects; you have to provide
-- at least a way to fetch the objects. Then you can add functions for
-- modifying, deleting or reordering the objects. Not all operations
-- are necessarily supported by all views.
data Editable f a = Editable
  { get :: !(Document -> f (a, Maybe UserId))
  , modify :: !(Maybe (a -> ModifyCommand a -> Command))
  , delete :: !(Maybe (a -> Command))
  , reorder :: !(Maybe (a -> ReorderAction a -> Command))
  } deriving (Generic)

-- | Creates a minimal definition of an Editable; you can add
-- definitions for modifying, deleting and reordering later.
editable :: (Document -> f (a, Maybe UserId)) -> Editable f a
editable get = Editable get Nothing Nothing Nothing

withModify :: Editable f a -> a -> ModifyCommand a -> Maybe Command
withModify Editable{modify} a cmd = fmap (\f -> f a cmd) modify

withDelete :: Editable f a -> a -> Maybe Command
withDelete Editable{delete} a = fmap (\f -> f a) delete

withReorder :: Editable f a -> a -> ReorderAction a -> Maybe Command
withReorder Editable{reorder} a action = fmap (\f -> f a action) reorder

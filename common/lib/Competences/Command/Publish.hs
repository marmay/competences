{-# LANGUAGE CPP #-}

module Competences.Command.Publish
  ( PublishData (..)
  , handlePublish
  )
where

import Competences.Command.Audience (CommandAudience (..))
import Competences.Command.Common (UpdateResult)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Task (..))
import Competences.Document.Assignment (Assignment (..))
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as IxSet
import GHC.Generics (Generic)
import Optics.Core ((&), (%~))

-- | Data for publishing draft entities into the real collections.
-- Contains full entity snapshots so students (who never received draft commands)
-- can reconstruct the entities from this single command.
data PublishData = PublishData
  { tasks :: ![Task]
  , assignment :: !(Maybe Assignment)
  }
  deriving (Eq, Generic, Show)

instance Binary PublishData
#ifdef WITH_AESON
instance FromJSON PublishData
instance ToJSON PublishData
#endif

-- | Handle a Publish command: insert entities into real collections, delete from drafts.
-- Tolerant: Ix.insert replaces existing (idempotent), Ix.deleteIx is no-op if not found.
handlePublish :: PublishData -> Document -> UpdateResult
handlePublish pd doc =
  let doc' =
        doc
          -- Insert real entities (Ix.insert replaces if ID exists — idempotent)
          & #tasks %~ (\s -> foldl' (flip Ix.insert) s pd.tasks)
          & #assignments %~ maybe id (\a s -> Ix.insert a s) pd.assignment
          -- Delete drafts (Ix.deleteIx is no-op if not found — tolerant)
          & #draftTasks %~ (\s -> foldl' (\s' t -> IxSet.deleteIx t.id s') s pd.tasks)
          & #draftAssignments %~ maybe id (\a s -> IxSet.deleteIx a.id s) pd.assignment
   in Right (doc', AudienceAll)

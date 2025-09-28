module Competences.Frontend.Component.EvidenceCreator
  ( evidenceCreatorComponent
  )
where

import Competences.Command (Command (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Evidence (..), EvidenceId, User, UserId)
import Competences.Document.Evidence (ActivityTasks (..), ActivityType (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.UserSelector (userSelectorComponent)
import Competences.Frontend.SyncDocument (SyncDocumentRef, modifySyncDocument)
import Competences.Frontend.View qualified as V
import Data.Set qualified as Set
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((.~), (&))

data Model = Model
  { userIds :: ![UserId]
  , date :: !Day
  , activityType :: !ActivityType
  , activityTasks :: !ActivityTasks
  }
  deriving (Eq, Generic, Show)

data Action
  = UpdateUserIds ![UserId]
  | UpdateDate !Day
  | UpdateActivityType !ActivityType
  | UpdateActivityTasks !ActivityTasks
  | CreateEvidence
  deriving (Eq, Show)

evidenceCreatorComponent
  :: SyncDocumentRef -> User -> EvidenceId -> Day -> M.Component p Model Action
evidenceCreatorComponent r u eId initialDay =
  M.component model update view
  where
    model = Model [] initialDay SemiSupervised (ActivityTasks "")

    update (UpdateUserIds userIds) = M.modify $ #userIds .~ userIds
    update (UpdateDate date) = M.modify $ #date .~ date
    update (UpdateActivityType activityType) = M.modify $ #activityType .~ activityType
    update (UpdateActivityTasks activityTasks) = M.modify $ #activityTasks .~ activityTasks
    update CreateEvidence = do
      e <- mkEvidence
      M.io_ $ modifySyncDocument r (AddEvidence e)

    mkEvidence = do
      m <- M.get
      pure $
        Evidence
          { id = eId
          , userIds = Set.fromList m.userIds
          , date = m.date
          , activityType = m.activityType
          , activityTasks = m.activityTasks
          , observations = Ix.empty
          }

    view m =
      V.viewFlow
        (V.vFlow & #gap .~ V.SmallSpace)
        [ V.title_ (C.translate' C.LblCreateEvidence)
        -- , M.div_ [M.key_ $ "user-selector:" <> show eId]
        --     M.+> (userSelectorComponent r)
        ]

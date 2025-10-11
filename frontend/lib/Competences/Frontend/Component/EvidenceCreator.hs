module Competences.Frontend.Component.EvidenceCreator
  ( evidenceCreatorComponent
  )
where

import Competences.Command (Command (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Evidence (..), EvidenceId, User (..), UserId)
import Competences.Document.Evidence (ActivityTasks (..), ActivityType (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.DateSelector (dateSelectorComponent)
import Competences.Frontend.Component.UserSelector (multiUserSelectorComponent)
import Competences.Frontend.SyncDocument (SyncDocumentRef, SyncDocumentEnv(..), modifySyncDocument, syncDocumentEnv)
import Competences.Frontend.View qualified as V
import Data.Set qualified as Set
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (.~), (^.))

data Model = Model
  { users :: ![User]
  , date :: !Day
  , activityType :: !ActivityType
  , activityTasks :: !ActivityTasks
  }
  deriving (Eq, Generic, Show)

data Action
  = UpdateDate !Day
  | UpdateActivityType !ActivityType
  | UpdateActivityTasks !ActivityTasks
  | CreateEvidence
  deriving (Eq, Show)

evidenceCreatorComponent
  :: SyncDocumentRef -> EvidenceId -> M.Component p Model Action
evidenceCreatorComponent r eId =
  M.component model update view
  where
    initialDay = syncDocumentEnv r ^. #currentDay

    model = Model [] initialDay SemiSupervised (ActivityTasks "")

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
          , userIds = Set.fromList $ map (^. #id) m.users
          , date = m.date
          , activityType = m.activityType
          , activityTasks = m.activityTasks
          , observations = Ix.empty
          }

    view m =
      V.viewFlow
        (V.vFlow & #gap .~ V.SmallSpace)
        [ V.title_ (C.translate' C.LblCreateEvidence)
        , V.mounted
            (M.ms $ "user-selector:" <> show eId)
            (multiUserSelectorComponent r (const True) #users)
        , V.mounted
            (M.ms $ "date-selector" <> show eId)
            (dateSelectorComponent initialDay #date)
        ]

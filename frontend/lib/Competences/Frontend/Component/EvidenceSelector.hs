module Competences.Frontend.Component.EvidenceSelector
  ( evidenceSelectorComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Evidence
  , EvidenceIxs
  , User, UserIxs
  )
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((.~), (&))

data DateRange
  = Today
  | ThisWeek
  | AllTime
  deriving (Eq, Show)

data Model = Model
  { filteredUsers :: ![User]
  , filteredDateRange :: !DateRange
  , allEvidenes :: Ix.IxSet EvidenceIxs Evidence
  }
  deriving (Eq, Generic, Show)

data Action

evidenceSelectorComponent :: SyncDocumentRef -> M.Component p Model Action
evidenceSelectorComponent r =
  M.component model update view
  where
    model = Model [] ThisWeek Ix.empty
    update = undefined
    view _ = V.viewFlow (V.vFlow & (#gap .~ V.SmallSpace))
               [ V.title_ "Evidences"
               ]

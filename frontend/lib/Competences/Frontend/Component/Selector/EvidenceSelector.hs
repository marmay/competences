module Competences.Frontend.Component.Selector.EvidenceSelector
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
import Optics.Core ((.~), (&), Lens', lensVL, toLensVL)

data DateRange
  = Today
  | ThisWeek
  | AllTime
  deriving (Eq, Show)

data Model = Model
  { filteredUsers :: ![User]
  , filteredDateRange :: !DateRange
  , allEvidenes :: Ix.IxSet EvidenceIxs Evidence
  , selectedEvidence :: !(Maybe Evidence)
  }
  deriving (Eq, Generic, Show)

data Action

evidenceSelectorComponent :: SyncDocumentRef -> Lens' p (Maybe Evidence) -> M.Component p Model Action
evidenceSelectorComponent r parentLens =
  (M.component model update view)
  { M.bindings = [ toLensVL parentLens M.<--- toLensVL #selectedEvidence ]
  }
    
  where
    model = Model [] ThisWeek Ix.empty Nothing
    update = undefined
    view _ = V.viewFlow (V.vFlow & (#gap .~ V.SmallSpace))
               [ V.title_ "Evidences"
               ]

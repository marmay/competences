module Competences.Frontend.Page.EvidencesPage
  ( evidencesPage
  , EvidencesPage
  , EvidencesView
  )
where

import Competences.Document.Evidence (EvidenceId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (.~))
import Competences.Frontend.Component.EvidenceSelector

data Model = Model
  { evidenceSelectorVisible :: !Bool
  , mainView :: !MainView
  }
  deriving (Eq, Generic, Show)

data MainView
  = EditEvidence !EvidenceId
  | CreateEvidence !EvidenceId
  | Empty
  deriving (Eq, Show)

data Action
  = SelectEvidence !EvidenceId
  deriving (Eq, Show)

type EvidencesPage p = M.Component p Model Action

type EvidencesView = M.View Model Action

evidencesPage :: SyncDocumentRef -> M.Component p Model Action
evidencesPage r =
  M.component model update view
  where
    model = Model True Empty
    update _ = pure ()
    view m =
      V.viewFlow
        ( V.hFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#expandOrthogonal .~ V.Expand V.Start)
            & (#gap .~ V.LargeSpace)
        )
        [ V.vScrollable (V.mounted "evidence-selector" $ evidenceSelectorComponent r)
        , V.vBorder
        , V.vScrollable (mainView)
        ]
      where
        mainView = case m.mainView of
          EditEvidence eid -> V.title_ (M.ms $ show eid)
          CreateEvidence eid -> V.title_ (M.ms $ show eid)
          Empty -> V.empty

module Competences.Frontend.Page.EvidencesPage
  ( evidencesPage
  , EvidencesPage
  , EvidencesView
  )
where

import Competences.Document.Evidence (EvidenceId, Evidence)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (.~), lens, Lens')
import Competences.Frontend.Component.EvidenceSelector

data Model
  = EditEvidence !Evidence
  | CreateEvidence !EvidenceId
  | Empty
  deriving (Eq, Show)

selectedEvidence :: Lens' Model (Maybe Evidence)
selectedEvidence = lens get set
  where
    get (EditEvidence eId) = Just eId
    get _ = Nothing
    set _ (Just eId) = EditEvidence eId
    set (EditEvidence _) Nothing = Empty
    set m Nothing = m

data Action

type EvidencesPage p = M.Component p Model Action

type EvidencesView = M.View Model Action

evidencesPage :: SyncDocumentRef -> M.Component p Model Action
evidencesPage r =
  M.component model update view
  where
    model = Empty
    update _ = pure ()
    view m =
      V.viewFlow
        ( V.hFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#expandOrthogonal .~ V.Expand V.Start)
            & (#gap .~ V.LargeSpace)
        )
        [ V.vScrollable (V.mounted "evidence-selector" $ evidenceSelectorComponent r selectedEvidence)
        , V.vBorder
        , V.vScrollable mainView
        ]
      where
        mainView = case m of
          EditEvidence eid -> V.title_ (M.ms $ show eid)
          CreateEvidence eid -> V.title_ (M.ms $ show eid)
          Empty -> V.empty

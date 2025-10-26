module Competences.Frontend.Page.EvidencesPage
  ( evidencesPage
  , EvidencesPage
  , EvidencesView
  )
where

import Competences.Document.Evidence (Evidence, EvidenceId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.EvidenceCreator (evidenceCreatorComponent)
import Competences.Frontend.Component.Selector.EvidenceSelector
import Competences.Frontend.SyncDocument (SyncDocumentRef, nextId)
import Competences.Frontend.View qualified as V
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core (Lens', lens, (&), (.~))

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
  = NewEvidence
  | NewEvidence' EvidenceId
  deriving (Eq, Show, Generic)

type EvidencesPage p = M.Component p Model Action

type EvidencesView = M.View Model Action

evidencesPage :: SyncDocumentRef -> M.Component p Model Action
evidencesPage r =
  M.component model update view
  where
    model = Empty
    update NewEvidence = do
      M.io $ NewEvidence' <$> nextId r
    update (NewEvidence' eId) =
      M.modify $ const $ CreateEvidence eId
    view m =
      V.viewFlow
        ( V.hFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#expandOrthogonal .~ V.Expand V.Start)
            & (#gap .~ V.LargeSpace)
        )
        [ V.viewFlow
            ( V.vFlow
                & (#expandDirection .~ V.Expand V.Start)
                & (#gap .~ V.SmallSpace)
            )
            [ V.viewButton (V.labelButton' C.LblAddEvidence NewEvidence)
            , V.vScrollable (V.mounted "evidence-selector" $ evidenceSelectorComponent r selectedEvidence)
            ]
        , V.vBorder
        , V.vScrollable mainView
        ]
      where
        mainView = case m of
          EditEvidence eid -> V.title_ (M.ms $ show eid)
          CreateEvidence eId ->
            V.mounted
              (M.ms $ "evidence-creator:" <> show eId)
              (evidenceCreatorComponent r eId)
          Empty -> V.empty

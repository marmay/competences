module Competences.Frontend.Page.ViewCompetenceGridPage
  ( viewCompetenceGridPage
  , ViewCompetenceGridView
  )
where

import Competences.Document (User)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Frontend.Common qualified as C
-- import Competences.Frontend.Component.EvidenceEditor (evidenceEditorComponent)
import Competences.Frontend.Component.CompetenceGridViewer (competenceGridViewerComponent)
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((&), (.~))

type ViewCompetenceGridView = M.View Model Action

data Model = Model
  { highlightedCompetenceLevels :: ![CompetenceLevelId]
  , sidePanel :: !SidePanel
  }
  deriving (Eq, Generic, Show)

data Action
  = SetSidePanel SidePanel
  deriving (Eq, Generic, Show)

data SidePanel
  = EvidenceViewer !CompetenceLevelId !Bool
  | EvidenceEditor
  | Menu
  deriving (Eq, Generic, Show)

viewCompetenceGridPage :: SyncDocumentRef -> M.Component p Model Action
viewCompetenceGridPage r = M.component model update view
  where
    model =
      Model
        { highlightedCompetenceLevels = []
        , sidePanel = Menu
        }

    update (SetSidePanel sidePanel) = M.modify (#sidePanel .~ sidePanel)

    view :: Model -> M.View Model Action
    view m =
      V.viewFlow
        ( V.hFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#gap .~ V.SmallSpace)
        )
        [ V.mounted' (competenceGridViewerComponent r)
        , V.flowSpring
        , viewSidePanel m.sidePanel
        ]

    viewSidePanel (EvidenceViewer _ _) = V.text_ "Not implemented yet"
    viewSidePanel EvidenceEditor = V.text_ "Not implemented yet"
    -- closableSidePanel "evidence-editor" False $
    --   evidenceEditorComponent r u
    viewSidePanel Menu =
      V.sidePanel
        V.MenuPanel
        [ V.viewButton $ V.iconButton' V.IcnAdd C.LblAddEvidence (SetSidePanel EvidenceEditor)
        ]

    closableSidePanel
      :: (Eq childModel)
      => M.MisoString -> Bool -> M.Component Model childModel childAction -> M.View Model Action
    closableSidePanel id' maximized child =
      V.sidePanel
        (if maximized then V.LargePanel else V.EditorPanel)
        [ V.viewButton $ V.iconButton' V.IcnCancel C.LblCancel (SetSidePanel Menu)
        , M.div_ [M.key_ id'] M.+> child
        ]

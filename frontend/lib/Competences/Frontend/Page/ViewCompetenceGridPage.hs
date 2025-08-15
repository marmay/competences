module Competences.Frontend.Page.ViewCompetenceGridPage
  ( viewCompetenceGridPage
  , ViewCompetenceGridView
  )
where

import Competences.Document (User)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.EvidenceEditor (evidenceEditorComponent)
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((.~))

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

competenceGridViewerComponent :: SyncDocumentRef -> User -> M.Component p () ()
competenceGridViewerComponent = undefined

viewCompetenceGridPage :: SyncDocumentRef -> User -> M.Component p Model Action
viewCompetenceGridPage r u = M.component model update view
  where
    model =
      Model
        { highlightedCompetenceLevels = []
        , sidePanel = Menu
        }

    update (SetSidePanel sidePanel) = M.modify (#sidePanel .~ sidePanel)

    view :: Model -> M.View Model Action
    view m =
      V.hBox_
        (V.Expand V.Start)
        V.NoExpand
        V.SmallGap
        [ V.mounted' (competenceGridViewerComponent r u)
        , V.spring_
        , viewSidePanel m.sidePanel
        ]

    viewSidePanel (EvidenceViewer _ _) = V.text_ "Not implemented yet"
    viewSidePanel EvidenceEditor =
      closableSidePanel "evidence-editor" False $
        evidenceEditorComponent r u
    viewSidePanel Menu =
      V.sidePanel
        V.MenuPanel
        [ V.iconButton
            [M.onClick $ SetSidePanel EvidenceEditor]
            V.RegularButton
            V.IcnAdd
            (C.translate' C.LblAddEvidence)
        ]

    closableSidePanel
      :: (Eq childModel)
      => M.MisoString -> Bool -> M.Component Model childModel childAction -> M.View Model Action
    closableSidePanel id' maximized child =
      V.sidePanel
        (if maximized then V.LargePanel else V.EditorPanel)
        [ V.iconButton
            [M.onClick $ SetSidePanel Menu]
            V.RegularButton
            V.IcnCancel
            (C.translate' C.LblCancel)
        , M.div_ [M.id_ id'] M.+> child
        ]

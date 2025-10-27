module Competences.Frontend.App
  ( runApp
  , mkApp
  , withTailwindPlay
  )
where

import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGridEditor (competenceGridEditorComponent)
import Competences.Frontend.Component.CompetenceGridViewer (competenceGridViewerComponent)
import Competences.Frontend.Component.EvidenceEditor (evidenceEditorComponent)
import Competences.Frontend.Component.UserListEditor (userListEditorComponent)
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import Data.Functor (($>))
import GHC.Generics (Generic)
import Language.Javascript.JSaddle (JSM)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Router qualified as M
import Optics.Core ((&), (.~), (^.))

type App = M.Component M.ROOT Model Action

newtype Model = Model
  { uri :: M.URI
  }
  deriving (Eq, Generic, Show)

data Action
  = PushURI M.URI
  | SetURI M.URI
  deriving (Eq, Show)

runApp :: App -> JSM ()
runApp = M.startComponent

mkApp :: SyncDocumentRef -> App
mkApp r =
  (M.component model update view) {M.subs = [M.uriSub SetURI]}
  where
    model = Model $ M.toURI EditCompetenceGrid

    update (SetURI uri) = M.modify $ #uri .~ uri
    update (PushURI uri) = M.io_ $ M.pushURI uri

    view m =
      M.div_
        []
        [ V.iconDefs
        , V.viewFlow
            ( V.vFlow
                & (#expandDirection .~ V.Expand V.Start)
                & (#expandOrthogonal .~ V.Expand V.Center)
                & (#gap .~ V.SmallSpace)
            )
            [title, topMenu, page (m ^. #uri), footer]
        ]

    title = V.title_ $ C.translate' C.LblPageTitle
    topMenu =
      V.viewFlow
        ( V.hFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#gap .~ V.SmallSpace)
        )
        [ V.viewButtons
            (V.hButtons & (#alignment .~ V.Center))
            [ V.labelButton' C.LblViewCompetenceGrid (PushURI $ M.toURI ViewCompetenceGrid)
            , V.labelButton' C.LblEditCompetenceGrid (PushURI $ M.toURI EditCompetenceGrid)
            , V.labelButton' C.LblEvidences (PushURI $ M.toURI Evidences)
            , V.labelButton' C.LblManageUsers (PushURI $ M.toURI ManageUsers)
            ]
        ]

    page uri = case M.route uri of
      Left _ -> V.text_ "404"
      Right v -> case v of
        ViewCompetenceGrid -> viewCompetenceGrid
        EditCompetenceGrid -> editCompetenceGrid
        Evidences -> evidences
        ManageUsers -> manageUsers

    viewCompetenceGrid = mounted ViewCompetenceGrid $ competenceGridViewerComponent r
    editCompetenceGrid = mounted EditCompetenceGrid $ competenceGridEditorComponent r
    evidences = mounted Evidences $ evidenceEditorComponent r
    manageUsers = mounted ManageUsers $ userListEditorComponent r

    mounted key c = M.div_ [M.key_ key] M.+> c

    footer = V.viewFlow (V.hFlow & (#expandDirection .~ V.Expand V.Center)) [V.text_ "© 2025 Markus Mayr"]

withTailwindPlay :: App -> App
withTailwindPlay app = app {M.scripts = M.Src "https://cdn.tailwindcss.com" : M.scripts app}

data Page
  = ViewCompetenceGrid
  | EditCompetenceGrid
  | Evidences
  | ManageUsers
  deriving (Eq, Show)

instance M.Router Page where
  routeParser =
    M.routes
      [ M.path "view-grid" $> ViewCompetenceGrid
      , M.path "edit-grid" $> EditCompetenceGrid
      , M.path "evidences" $> Evidences
      , M.path "users" $> ManageUsers
      ]
  fromRoute ViewCompetenceGrid = [M.toPath "view-grid"]
  fromRoute EditCompetenceGrid = [M.toPath "edit-grid"]
  fromRoute Evidences = [M.toPath "evidences"]
  fromRoute ManageUsers = [M.toPath "users"]

instance M.ToKey Page where
  toKey = M.toKey . show

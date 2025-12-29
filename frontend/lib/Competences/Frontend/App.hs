module Competences.Frontend.App
  ( runApp
  , mkApp
  , withTailwindPlay
  )
where

import Competences.Document.User (User, isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGridEditor (competenceGridEditorComponent)
import Competences.Frontend.Component.CompetenceGridViewer (competenceGridViewerComponent)
import Competences.Frontend.Component.EvidenceEditor (evidenceEditorComponent)
import Competences.Frontend.Component.SelfContainedTaskEditor (selfContainedTaskEditorComponent)
import Competences.Frontend.Component.StatisticsOverview (statisticsOverviewComponent)
import Competences.Frontend.Component.StatisticsViewer (statisticsViewerComponent)
import Competences.Frontend.Component.UserListEditor (userListEditorComponent)
import Competences.Frontend.SyncDocument (SyncDocumentEnv (..), SyncDocumentRef, syncDocumentEnv)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind qualified as T
import Data.Functor (($>))
import GHC.Generics (Generic)
import Language.Javascript.JSaddle (JSM)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Router qualified as M
import Optics.Core ((&), (.~), (^.))

type App = M.Component M.ROOT Model Action

data Model = Model
  { uri :: M.URI
  , connectedUser :: User
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
    env = syncDocumentEnv r
    model = Model {uri = M.toURI EditCompetenceGrid, connectedUser = env ^. #connectedUser}

    update (SetURI uri) = M.modify $ #uri .~ uri
    update (PushURI uri) = M.io_ $ M.pushURI uri

    view :: Model -> M.View Model Action
    view m =
      M.div_
        [T.tailwind [T.HScreen]]
        [ V.iconDefs
        , V.viewFlow
            ( V.vFlow
                & (#expandDirection .~ V.Expand V.Start)
                & (#expandOrthogonal .~ V.Expand V.Center)
                & (#gap .~ V.SmallSpace)
                & (#extraAttrs .~ [T.tailwind [T.HFull]])
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
            ( if isTeacher model.connectedUser
                then
                  [ V.labelButton' C.LblViewCompetenceGrid (PushURI $ M.toURI ViewCompetenceGrid)
                  , V.labelButton' C.LblEditCompetenceGrid (PushURI $ M.toURI EditCompetenceGrid)
                  , V.labelButton' C.LblEvidences (PushURI $ M.toURI Evidences)
                  , V.labelButton' C.LblSelfContainedTasks (PushURI $ M.toURI ManageTasks)
                  , V.labelButton' C.LblStatisticsOverview (PushURI $ M.toURI StatisticsOverview)
                  , V.labelButton' C.LblStatisticsIndividual (PushURI $ M.toURI StatisticsIndividual)
                  , V.labelButton' C.LblManageUsers (PushURI $ M.toURI ManageUsers)
                  ]
                else
                  [ V.labelButton' C.LblViewCompetenceGrid (PushURI $ M.toURI ViewCompetenceGrid)
                  , V.labelButton' C.LblEvidences (PushURI $ M.toURI Evidences)
                  , V.labelButton' C.LblStatisticsIndividual (PushURI $ M.toURI StatisticsIndividual)
                  ]
            )
        ]

    page uri = case M.route uri of
      Left _ -> V.text_ "404"
      Right v -> case v of
        ViewCompetenceGrid -> viewCompetenceGrid
        EditCompetenceGrid -> editCompetenceGrid
        Evidences -> evidences
        ManageTasks -> manageTasks
        StatisticsOverview -> statisticsOverview
        StatisticsIndividual -> statisticsIndividual
        ManageUsers -> manageUsers

    viewCompetenceGrid = mounted ViewCompetenceGrid $ competenceGridViewerComponent r
    editCompetenceGrid = mounted EditCompetenceGrid $ competenceGridEditorComponent r
    evidences = mounted Evidences $ evidenceEditorComponent r
    manageTasks = mounted ManageTasks $ selfContainedTaskEditorComponent r
    statisticsOverview = mounted StatisticsOverview $ statisticsOverviewComponent r
    statisticsIndividual = mounted StatisticsIndividual $ statisticsViewerComponent r model.connectedUser
    manageUsers = mounted ManageUsers $ userListEditorComponent r

    mounted key c = M.div_ [M.key_ key, T.tailwind [T.MinH0]] M.+> c

    footer = V.viewFlow (V.hFlow & (#expandDirection .~ V.Expand V.Center)) [V.text_ "© 2025 Markus Mayr"]

withTailwindPlay :: App -> App
withTailwindPlay app = app {M.scripts = M.Src "https://cdn.tailwindcss.com" : M.scripts app}

data Page
  = ViewCompetenceGrid
  | EditCompetenceGrid
  | Evidences
  | ManageTasks
  | StatisticsOverview
  | StatisticsIndividual
  | ManageUsers
  deriving (Eq, Show)

instance M.Router Page where
  routeParser =
    M.routes
      [ M.path "view-grid" $> ViewCompetenceGrid
      , M.path "edit-grid" $> EditCompetenceGrid
      , M.path "evidences" $> Evidences
      , M.path "tasks" $> ManageTasks
      , M.path "statistics-overview" $> StatisticsOverview
      , M.path "statistics-individual" $> StatisticsIndividual
      , M.path "users" $> ManageUsers
      ]
  fromRoute ViewCompetenceGrid = [M.toPath "view-grid"]
  fromRoute EditCompetenceGrid = [M.toPath "edit-grid"]
  fromRoute Evidences = [M.toPath "evidences"]
  fromRoute ManageTasks = [M.toPath "tasks"]
  fromRoute StatisticsOverview = [M.toPath "statistics-overview"]
  fromRoute StatisticsIndividual = [M.toPath "statistics-individual"]
  fromRoute ManageUsers = [M.toPath "users"]

instance M.ToKey Page where
  toKey = M.toKey . show

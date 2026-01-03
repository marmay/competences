module Competences.Frontend.App
  ( runApp
  , mkApp
  , withTailwindPlay
  )
where

import Competences.Document.User (User, isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Assignment (assignmentComponent)
import Competences.Frontend.Component.AssignmentViewer (assignmentViewerComponent)
import Competences.Frontend.Component.CompetenceGrid (CompetenceGridMode (..), competenceGridComponent)
import Data.List.NonEmpty (NonEmpty (..))
import Competences.Frontend.Component.EvidenceEditor (evidenceEditorComponent)
import Competences.Frontend.Component.SelfContainedTaskEditor (selfContainedTaskEditorComponent)
import Competences.Frontend.Component.StatisticsOverview (statisticsOverviewComponent)
import Competences.Frontend.Component.UserListEditor (userListEditorComponent)
import Competences.Frontend.SyncDocument (SyncDocumentEnv (..), SyncDocumentRef, syncDocumentEnv)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Component (componentA)
import Data.Functor (($>))
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Router qualified as M
import Optics.Core ((.~), (^.), (&))

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

runApp :: App -> IO ()
runApp = M.startComponent

mkApp :: SyncDocumentRef -> App
mkApp r =
  (M.component model update view) {M.subs = [M.uriSub SetURI]}
  where
    env = syncDocumentEnv r
    model = Model {uri = M.toURI CompetenceGrid, connectedUser = env ^. #connectedUser}

    update (SetURI uri) = M.modify $ #uri .~ uri
    update (PushURI uri) = M.io_ $ M.pushURI uri

    view :: Model -> M.View Model Action
    view m =
      M.div_
        []
        [ V.iconDefs
        , V.mainPage
            (C.translate' C.LblPageTitle)
            (navButtons m)
            (page (m ^. #uri))
            (M.text "© 2025 Markus Mayr")
        ]

    navButtons m' =
      if isTeacher m'.connectedUser
        then
          [ navButton C.LblCompetenceGrid CompetenceGrid
          , navButton C.LblEvidences Evidences
          , navButton C.LblSelfContainedTasks ManageTasks
          , navButton C.LblAssignments ManageAssignments
          , navButton C.LblStatisticsOverview StatisticsOverview
          , navButton C.LblManageUsers ManageUsers
          ]
        else
          [ navButton C.LblCompetenceGrid CompetenceGrid
          , navButton C.LblEvidences Evidences
          , navButton C.LblAssignments ViewAssignments
          ]

    navButton lbl p =
      Button.buttonSecondary (C.translate' lbl)
        & Button.withClick (PushURI $ M.toURI p)
        & Button.renderButton

    page uri = case M.route uri of
      Left _ -> V.text_ "404"
      Right v -> case v of
        CompetenceGrid -> competenceGrid
        Evidences -> evidences
        ManageTasks -> manageTasks
        ViewAssignments -> viewAssignments
        ManageAssignments -> manageAssignments
        StatisticsOverview -> statisticsOverview
        ManageUsers -> manageUsers

    competenceGrid = mounted CompetenceGrid $ competenceGridComponent r defaultGridMode availableGridModes
    defaultGridMode = if isTeacher model.connectedUser then GridEdit else GridView
    availableGridModes =
      if isTeacher model.connectedUser
        then GridView :| [GridEdit]
        else GridView :| []
    evidences = mounted Evidences $ evidenceEditorComponent r
    manageTasks = mounted ManageTasks $ selfContainedTaskEditorComponent r
    viewAssignments = mounted ViewAssignments $ assignmentViewerComponent r model.connectedUser
    manageAssignments = mounted ManageAssignments $ assignmentComponent r
    statisticsOverview = mounted StatisticsOverview $ statisticsOverviewComponent r
    manageUsers = mounted ManageUsers $ userListEditorComponent r

    mounted key = componentA (M.ms $ show key) [V.minH0]

-- | No-op function (CSS is now loaded in HTML head via backend)
-- Kept for backward compatibility with Main.hs
withTailwindPlay :: App -> App
withTailwindPlay = id

data Page
  = CompetenceGrid
  | Evidences
  | ManageTasks
  | ViewAssignments
  | ManageAssignments
  | StatisticsOverview
  | ManageUsers
  deriving (Eq, Show)

instance M.Router Page where
  routeParser =
    M.routes
      [ M.path "grid" $> CompetenceGrid
      , M.path "evidences" $> Evidences
      , M.path "tasks" $> ManageTasks
      , M.path "assignments" $> ViewAssignments
      , M.path "manage-assignments" $> ManageAssignments
      , M.path "statistics-overview" $> StatisticsOverview
      , M.path "users" $> ManageUsers
      ]
  fromRoute CompetenceGrid = [M.toPath "grid"]
  fromRoute Evidences = [M.toPath "evidences"]
  fromRoute ManageTasks = [M.toPath "tasks"]
  fromRoute ViewAssignments = [M.toPath "assignments"]
  fromRoute ManageAssignments = [M.toPath "manage-assignments"]
  fromRoute StatisticsOverview = [M.toPath "statistics-overview"]
  fromRoute ManageUsers = [M.toPath "users"]

instance M.ToKey Page where
  toKey = M.toKey . show

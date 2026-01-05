module Competences.Frontend.App
  ( runApp
  , mkApp
  , withTailwindPlay
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..))
import Competences.Document.User (isStudent, isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Assignment (assignmentComponent)
import Competences.Frontend.Component.AssignmentViewer (assignmentViewerComponent)
import Competences.Frontend.Component.CompetenceGrid (CompetenceGridMode (..), competenceGridComponent)
import Competences.Frontend.Component.EvidenceEditor (evidenceEditorComponent)
import Competences.Frontend.Component.StatisticsOverview (statisticsOverviewComponent)
import Competences.Frontend.Component.TaskEditor (taskEditorComponent)
import Competences.Frontend.Component.UserListEditor (userListEditorComponent)
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , FocusedUserChange (..)
  , SyncDocumentEnv (..)
  , SyncDocumentRef
  , setFocusedUser
  , subscribeDocument
  , subscribeFocusedUser
  , syncDocumentEnv
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Component (componentA)
import Competences.Frontend.View.Tailwind (class_)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as M
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

runApp :: App -> IO ()
runApp = M.startComponent

mkApp :: SyncDocumentRef -> App
mkApp r =
  (M.component model update view)
    { M.subs = [M.uriSub SetURI]
    , M.events = M.defaultEvents <> M.keyboardEvents
    }
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
            (focusedUserView r)
            (navButtons m)
            (page (m ^. #uri))
            (M.text "© 2025-2026 Markus Mayr")
        ]

    navButtons m' =
      let currentPage = M.route (m' ^. #uri)
          nb = navButton currentPage
       in if isTeacher m'.connectedUser
            then
              [ nb C.LblCompetenceGrid CompetenceGrid
              , nb C.LblEvidences Evidences
              , nb C.LblSelfContainedTasks ManageTasks
              , nb C.LblAssignments ManageAssignments
              , nb C.LblStatisticsOverview StatisticsOverview
              , nb C.LblManageUsers ManageUsers
              ]
            else
              [ nb C.LblCompetenceGrid CompetenceGrid
              , nb C.LblEvidences Evidences
              , nb C.LblAssignments ViewAssignments
              ]

    navButton currentPage lbl p =
      let isActive = currentPage == Right p
       in -- Active button gets a subtle white background wrapper
          if isActive
            then M.span_
                   [class_ "bg-white/20 rounded-md"]
                   [ Button.buttonGhost (C.translate' lbl)
                       & Button.withClick (PushURI $ M.toURI p)
                       & Button.renderButton
                   ]
            else Button.buttonSecondary (C.translate' lbl)
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
    manageTasks = mounted ManageTasks $ taskEditorComponent r
    viewAssignments = mounted ViewAssignments $ assignmentViewerComponent r model.connectedUser
    manageAssignments = mounted ManageAssignments $ assignmentComponent r
    statisticsOverview = mounted StatisticsOverview $ statisticsOverviewComponent r
    manageUsers = mounted ManageUsers $ userListEditorComponent r

    mounted key = componentA (M.ms $ show key) [V.minH0, V.fullWidth, V.fullHeight]

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

-- ============================================================================
-- FOCUSED USER VIEW (Nav bar component)
-- ============================================================================

-- | View for the focused user in the nav bar
-- For students: displays their name as static text
-- For teachers: shows a searchable selector for choosing any student
focusedUserView :: SyncDocumentRef -> M.View p a
focusedUserView r = V.component "focused-user" (focusedUserComponent r)

-- | Model for the focused user component
data FocusedUserModel = FocusedUserModel
  { focusedUser :: !(Maybe User)
  , allStudents :: ![User]
  , searchText :: !Text
  , isDropdownOpen :: !Bool
  , connectedUser :: !User
  }
  deriving (Eq, Generic, Show)

-- | Action for the focused user component
data FocusedUserAction
  = FocusedUserChanged !FocusedUserChange
  | DocumentUpdated !DocumentChange
  | SetSearchText !Text
  | OpenDropdown
  | CloseDropdown
  | SelectUser !(Maybe User)
  deriving (Eq, Show)

-- | Focused user component that shows a selector for teachers
focusedUserComponent :: SyncDocumentRef -> M.Component p FocusedUserModel FocusedUserAction
focusedUserComponent r =
  (M.component model update view)
    { M.subs =
        [ subscribeFocusedUser r FocusedUserChanged
        , subscribeDocument r DocumentUpdated
        ]
    }
  where
    env = syncDocumentEnv r

    model =
      FocusedUserModel
        { focusedUser = Nothing
        , allStudents = []
        , searchText = ""
        , isDropdownOpen = False
        , connectedUser = env.connectedUser
        }

    update (FocusedUserChanged change) =
      M.modify $ \m -> m & #focusedUser .~ change.user

    update (DocumentUpdated dc) =
      M.modify $ \m ->
        let students = filter isStudent $ Ix.toAscList (Proxy @Text) dc.document.users
         in m & #allStudents .~ students

    update (SetSearchText t) =
      M.modify $ \m -> m & #searchText .~ t

    update OpenDropdown =
      M.modify $ \m -> m & #isDropdownOpen .~ True

    update CloseDropdown =
      M.modify $ \m -> m & #isDropdownOpen .~ False & #searchText .~ ""

    update (SelectUser maybeUser) = do
      M.io_ $ setFocusedUser r maybeUser
      M.modify $ \m -> m & #isDropdownOpen .~ False & #searchText .~ ""

    view m
      | isStudent m.connectedUser = viewStudentFocusedUser m
      | otherwise = viewTeacherFocusedUser m

-- | View for students: just shows their name
viewStudentFocusedUser :: FocusedUserModel -> M.View FocusedUserModel FocusedUserAction
viewStudentFocusedUser m =
  M.span_
    [class_ "text-lg font-medium"]
    [M.text $ M.ms $ maybe "" (.name) m.focusedUser]

-- | View for teachers: searchable dropdown selector
viewTeacherFocusedUser :: FocusedUserModel -> M.View FocusedUserModel FocusedUserAction
viewTeacherFocusedUser m =
  M.div_
    [class_ "relative"]
    [ -- Button to open dropdown
      M.button_
        [ class_ "flex items-center gap-2 px-3 py-1 rounded bg-white/10 hover:bg-white/20 text-primary-foreground"
        , M.onClick OpenDropdown
        ]
        [ M.span_ [] [M.text $ focusedUserLabel m]
        , M.span_ [class_ "text-xs"] [M.text "▼"]
        ]
    , -- Dropdown menu (when open)
      if m.isDropdownOpen
        then viewDropdown m
        else M.text ""
    ]

-- | Label for the focused user button
focusedUserLabel :: FocusedUserModel -> M.MisoString
focusedUserLabel m = case m.focusedUser of
  Nothing -> C.translate' C.LblAllStudents
  Just u -> M.ms u.name

-- | Dropdown menu with search and student list
viewDropdown :: FocusedUserModel -> M.View FocusedUserModel FocusedUserAction
viewDropdown m =
  M.div_
    [ class_ "absolute right-0 top-full mt-1 z-50 min-w-64 bg-popover text-popover-foreground border border-border rounded-md shadow-lg"
    ]
    [ -- Backdrop to close dropdown
      M.div_
        [ class_ "fixed inset-0 z-[-1]"
        , M.onClick CloseDropdown
        ]
        []
    , -- Search input
      M.div_
        [class_ "p-2 border-b border-border"]
        [ M.input_
            [ M.type_ "text"
            , M.placeholder_ (C.translate' C.LblFocusedStudent)
            , M.value_ (M.ms m.searchText)
            , M.onInput (SetSearchText . M.fromMisoString)
            , class_ "w-full px-2 py-1 text-sm border border-input rounded bg-background"
            ]
        ]
    , -- Option to clear selection (show all students)
      M.div_
        [class_ "max-h-64 overflow-y-auto"]
        ( viewAllStudentsOption m : map (viewStudentOption m) (filteredStudents m)
        )
    ]

-- | Option to show "All students" (clear focused user)
viewAllStudentsOption :: FocusedUserModel -> M.View FocusedUserModel FocusedUserAction
viewAllStudentsOption m =
  let isSelected = m.focusedUser == Nothing
      optionClass =
        if isSelected
          then "px-3 py-2 cursor-pointer bg-accent text-accent-foreground"
          else "px-3 py-2 cursor-pointer hover:bg-muted"
   in M.div_
        [ class_ optionClass
        , M.onClick (SelectUser Nothing)
        ]
        [M.text $ C.translate' C.LblAllStudents]

-- | Option for a single student
viewStudentOption :: FocusedUserModel -> User -> M.View FocusedUserModel FocusedUserAction
viewStudentOption m u =
  let isSelected = m.focusedUser == Just u
      optionClass =
        if isSelected
          then "px-3 py-2 cursor-pointer bg-accent text-accent-foreground"
          else "px-3 py-2 cursor-pointer hover:bg-muted"
   in M.div_
        [ class_ optionClass
        , M.onClick (SelectUser (Just u))
        ]
        [M.text $ M.ms u.name]

-- | Filter students by search text
filteredStudents :: FocusedUserModel -> [User]
filteredStudents m
  | Text.null m.searchText = m.allStudents
  | otherwise =
      let searchLower = Text.toLower m.searchText
       in filter (\u -> searchLower `Text.isInfixOf` Text.toLower u.name) m.allStudents

module Competences.Frontend.App
  ( runApp
  , mkApp
  )
where

import Competences.Document (User (..))
import Competences.Document.User (isStudent, isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Assignment (assignmentComponent)
import Competences.Frontend.Component.AboutDialog (aboutButtonView)
import Competences.Frontend.Component.CompetenceGrid (CompetenceGridMode (..), competenceGridComponent)
import Competences.Frontend.Component.ConnectionStatus (connectionStatusView)
import Competences.Frontend.Component.EvidenceEditor (evidenceEditorComponent)
import Competences.Frontend.Component.LessonNotes (LessonNotesMode (..), lessonNotesComponent)
import Competences.Frontend.Component.Planning (planningComponent)
import Competences.Frontend.Component.ResourceEditor (resourceEditorComponent)
import Competences.Frontend.Component.StatisticsOverview (statisticsOverviewComponent)
import Competences.Frontend.Component.TaskEditor (taskEditorComponent)
import Competences.Frontend.Component.UserListEditor (userListEditorComponent)
import Competences.Frontend.Component.WindowHost (windowHostComponent)
import Competences.Frontend.Page
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , FocusedUserChange (..)
  , SyncContext (..)
  , SyncDocumentEnv (..)
  , getFocusedUserRef
  , setFocusedUser
  , subscribeDocument
  , subscribeFocusedUser
  , syncDocumentEnv
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Component (componentA)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.NavBar qualified as NavBar
import Competences.Frontend.View.Tailwind (class_)
import Competences.Query.User qualified as QUser
import Data.List.NonEmpty (NonEmpty (..))
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

-- | Events configuration for the application
-- Includes defaultEvents, keyboardEvents, and mouseEvents for full event support
appEvents :: M.Events
appEvents = M.defaultEvents <> M.keyboardEvents <> M.mouseEvents

runApp :: App -> IO ()
runApp = M.startComponent appEvents

mkApp :: SyncContext -> App
mkApp ir =
  (M.component model update view)
    { M.subs = [M.uriSub SetURI]
    }
  where
    env = syncDocumentEnv ir
    model =
      Model
        { uri = M.toURI CompetenceGrid
        , connectedUser = env ^. #connectedUser
        }

    update (SetURI uri) = M.modify $ #uri .~ uri
    update (PushURI uri) = M.io_ $ M.pushURI uri

    view :: Model -> M.View Model Action
    view m =
      let currentPage = either (const CompetenceGrid) id $ M.route (m ^. #uri)
          teacher = isTeacher m.connectedUser
          categories = if teacher then NavBar.teacherCategories else NavBar.studentCategories
          extras = if teacher then NavBar.teacherExtraCategories else []
          navigate p = PushURI (M.toURI p)
       in M.div_
            [class_ "flex flex-row h-screen"]
            [ Icon.iconDefs
            , M.div_
                [class_ "flex-1 min-w-0 flex flex-col"]
                [ V.mainPage
                    (NavBar.burgerMenuView navigate currentPage categories extras)
                    (C.translate' C.LblPageTitle)
                    (map (NavBar.navCategoryView teacher navigate currentPage) categories)
                    (focusedUserView ir)
                    ( Layout.hFlow (Layout.gapS <> Layout.crossCenter)
                        [ aboutButtonView ir
                        , connectionStatusView ir
                        ]
                    )
                    (page (m ^. #uri))
                ]
            , M.div_ [class_ "print:hidden"] [V.component "window-host" (windowHostComponent ir.windowManager)]
            ]

    page uri = case M.route uri of
      Left _ -> V.text_ "404"
      Right v -> case v of
        CompetenceGrid -> competenceGrid
        Planning -> planning
        Evidences -> evidences
        ManageTasks -> manageTasks
        ManageResources -> manageResources
        ManageLessonNotes -> manageLessonNotes
        ViewAssignments -> viewAssignments
        ManageAssignments -> manageAssignments
        StatisticsOverview -> statisticsOverview
        ManageUsers -> manageUsers

    competenceGrid = mounted CompetenceGrid $ competenceGridComponent ir defaultGridMode availableGridModes
    defaultGridMode = GridView
    availableGridModes =
      if isTeacher model.connectedUser
        then GridView :| [GridEdit, GridAssessment, GridGrading]
        else GridView :| []
    planning = mounted Planning $ planningComponent ir
    evidences = mounted Evidences $ evidenceEditorComponent ir (isTeacher model.connectedUser)
    manageTasks = mounted ManageTasks $ taskEditorComponent ir
    manageResources = mounted ManageResources $ resourceEditorComponent ir
    manageLessonNotes = mounted ManageLessonNotes $ lessonNotesComponent ir lessonNotesDefaultMode lessonNotesAvailableModes lessonNotesCanCreate
    lessonNotesDefaultMode = if isTeacher model.connectedUser then LessonNotesEdit else LessonNotesView
    lessonNotesAvailableModes = if isTeacher model.connectedUser then LessonNotesEdit :| [LessonNotesView] else LessonNotesView :| []
    lessonNotesCanCreate = isTeacher model.connectedUser
    -- Both routes use the unified assignment component
    -- Teachers see Edit/Evaluate/View modes, students see View mode only
    viewAssignments = mounted ViewAssignments $ assignmentComponent ir model.connectedUser
    manageAssignments = mounted ManageAssignments $ assignmentComponent ir model.connectedUser
    statisticsOverview = mounted StatisticsOverview $ statisticsOverviewComponent ir
    manageUsers = mounted ManageUsers $ userListEditorComponent ir

    mounted key = componentA (M.ms $ show key) [class_ "min-h-0", class_ "w-full", class_ "h-full"]

-- ============================================================================
-- FOCUSED USER VIEW (Nav bar component)
-- ============================================================================

-- | View for the focused user in the nav bar
-- For students: displays their name as static text
-- For teachers: shows a searchable selector for choosing any student
focusedUserView :: SyncContext -> M.View p a
focusedUserView ir = V.component "focused-user" (focusedUserComponent ir)

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
focusedUserComponent :: SyncContext -> M.Component p FocusedUserModel FocusedUserAction
focusedUserComponent ir =
  (M.component model update view)
    { M.subs =
        [ subscribeFocusedUser (getFocusedUserRef ir) FocusedUserChanged
        , subscribeDocument ir DocumentUpdated
        ]
    }
  where
    env = syncDocumentEnv ir

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
        let students = QUser.studentsSortedByName dc.document
         in m & #allStudents .~ students

    update (SetSearchText t) =
      M.modify $ \m -> m & #searchText .~ t

    update OpenDropdown =
      M.modify $ \m -> m & #isDropdownOpen .~ True

    update CloseDropdown =
      M.modify $ \m -> m & #isDropdownOpen .~ False & #searchText .~ ""

    update (SelectUser maybeUser) = do
      M.io_ $ setFocusedUser (getFocusedUserRef ir) maybeUser
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

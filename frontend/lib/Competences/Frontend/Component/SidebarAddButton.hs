-- | A round "+" button mounted at the bottom of the pin sidebar.
--
-- Opens a hover menu that creates a new entity of the chosen kind via
-- the standard 'CreateAndLock' flow. The lock-watching machinery
-- already opens the editor pin for the new entity automatically, and
-- in @followUp = True@ mode (which it uses for live commands) records
-- the currently-visible pin as the parent — closing the new editor
-- restores the pin we came from.
--
-- This is intentionally a workaround: it lets a teacher spawn a
-- task / draft task / assignment / draft assignment / resource from
-- inside any open pin without first navigating to the corresponding
-- entity page. Refine when a more contextual pattern emerges.
module Competences.Frontend.Component.SidebarAddButton
  ( sidebarAddButton
  )
where

import Competences.Command
  ( AssignmentsCommand (..)
  , Command (..)
  , DraftAssignmentsCommand (..)
  , DraftTasksCommand (..)
  , EntityCommand (..)
  , ResourcesCommand (..)
  , TasksCommand (..)
  )
import Competences.Document (Resource (..), ResourceContent (..))
import Competences.Document.Assignment (AssignmentName (..), mkAssignment)
import Competences.Document.Resource (mkResource)
import Competences.Document.Task (defaultTask)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext
  ( SyncContext (..)
  , SyncDocumentEnv (..)
  , modifySyncDocument
  , nextId
  , syncDocumentEnv
  )
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.HoverMenu qualified as HoverMenu
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Miso qualified as M
import Miso.Html qualified as MH

-- | Mount the add button anywhere a 'View' is needed. Self-contained
-- inline component; the parent doesn't need to handle any actions.
sidebarAddButton :: SyncContext -> M.View parent action
sidebarAddButton r = inlineComponent "sidebar-add-button" (component r)

data Action
  = CreateNewTask
  | CreateNewDraftTask
  | CreateNewAssignment
  | CreateNewDraftAssignment
  | CreateNewResource
  deriving (Eq, Show)

component :: SyncContext -> M.Component parent () Action
component r = M.component () update view
  where
    update CreateNewTask = M.io_ $ do
      tid <- nextId r
      modifySyncDocument r $ Tasks (OnTasks (CreateAndLock (defaultTask tid)))
    update CreateNewDraftTask = M.io_ $ do
      tid <- nextId r
      modifySyncDocument r $ DraftTasks (OnDraftTasks (CreateAndLock (defaultTask tid)))
    update CreateNewAssignment = M.io_ $ do
      aid <- nextId r
      let today = (syncDocumentEnv r).currentDay
      modifySyncDocument r $
        Assignments (OnAssignments (CreateAndLock (mkAssignment aid (AssignmentName "") today)))
    update CreateNewDraftAssignment = M.io_ $ do
      aid <- nextId r
      let today = (syncDocumentEnv r).currentDay
      modifySyncDocument r $
        DraftAssignments (OnDraftAssignments (CreateAndLock (mkAssignment aid (AssignmentName "") today)))
    update CreateNewResource = M.io_ $ do
      rid <- nextId r
      let newResource = (mkResource rid) {content = InlineContent mempty}
      modifySyncDocument r $ Resources (OnResources (CreateAndLock newResource))

    view _ =
      HoverMenu.hoverMenuAboveRight trigger items
      where
        trigger =
          MH.div_
            [ class_
                "w-10 h-10 flex items-center justify-center rounded-full \
                \bg-popover text-popover-foreground border border-border shadow \
                \cursor-pointer hover:bg-accent hover:text-accent-foreground"
            ]
            [Icon.icon [class_ "w-5 h-5"] Icon.IcnAdd]
        items =
          [ HoverMenu.hoverMenuEntry False Icon.IcnTask (C.translate' C.LblNewTask) CreateNewTask
          , HoverMenu.hoverMenuEntry False Icon.IcnTask (C.translate' C.LblNewDraftTask) CreateNewDraftTask
          , HoverMenu.hoverMenuSeparator
          , HoverMenu.hoverMenuEntry False Icon.IcnAssignment (C.translate' C.LblNewAssignment) CreateNewAssignment
          , HoverMenu.hoverMenuEntry False Icon.IcnAssignment (C.translate' C.LblNewDraftAssignment) CreateNewDraftAssignment
          , HoverMenu.hoverMenuSeparator
          , HoverMenu.hoverMenuEntry False Icon.IcnResources (C.translate' C.LblAddResource) CreateNewResource
          ]

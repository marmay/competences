-- | Self-contained entity context menu component.
--
-- Takes a declarative 'EntityMenuConfig' describing what actions are available.
-- Manages its own state (menu open/close, lock status, hold-to-delete).
-- All effects (lock, pin, navigate, delete) are handled internally.
module Competences.Frontend.Component.EntityMenu
  ( EntityMenuConfig (..)
  , EditConfig (..)
  , DeleteConfig (..)
  , ExtraEntry (..)
  , entityMenuComponent
    -- * Smart constructors
  , taskEdit
  , taskDelete
  , resourceEdit
  , resourceDelete
  , lessonNotesEdit
  , lessonNotesDelete
  , assignmentEdit
  , assignmentDelete
  , lessonEdit
  , lessonDelete
  )
where

import Competences.Command (AssignmentsCommand (..), Command (..), EntityCommand (..), LessonsCommand (..), ModifyCommand (..), ResourcesCommand (..), TasksCommand (..))
import Competences.Command qualified as Cmd
import Competences.Command.LessonNotes (LessonNotesCommand (..))
import Competences.Document (Lock (..))
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Lesson (LessonId)
import Competences.Document.LessonNotes (LessonNotesId)
import Competences.Document.Resource (ResourceId)
import Competences.Document.Task (TaskId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Common.Effect (FragmentDef (..), liftEffect, liftEffect_, mapSub)
import Competences.Frontend.Component.Draft (EntityOrigin, wrapForOrigin)
import Competences.Frontend.Component.LockButton qualified as LB
import Competences.Frontend.Page (Page)
import Competences.Frontend.SyncContext
  ( PinViewerRequest
  , SyncContext
  , modifySyncDocument
  , requestViewerPin
  )
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.HoldButton qualified as HoldButton
import Competences.Frontend.View.HoverMenu qualified as HoverMenu
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Control.Monad (when)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Router qualified as M
import Miso.String (MisoString)

-- ============================================================================
-- Config
-- ============================================================================

data EntityMenuConfig = EntityMenuConfig
  { edit :: !(Maybe EditConfig)
  , pin :: !(Maybe PinViewerRequest)
  , goTo :: !(Maybe Page)
  , delete :: !(Maybe DeleteConfig)
  , extraEntries :: ![ExtraEntry]
  }

data EditConfig = EditConfig
  { lock :: !Lock
  , lockCommand :: !Command
  }

newtype DeleteConfig = DeleteConfig
  { deleteCommand :: Command
  }

data ExtraEntry = ExtraEntry
  { icon :: !Icon.Icon
  , label :: !MisoString
  , action :: !(IO ())
  }

-- ============================================================================
-- Smart constructors
-- ============================================================================

taskEdit :: TaskId -> EntityOrigin -> EditConfig
taskEdit tid origin = EditConfig (TaskLock tid) (wrapForOrigin origin $ Tasks (OnTasks (Modify tid Lock)))

taskDelete :: TaskId -> EntityOrigin -> DeleteConfig
taskDelete tid origin = DeleteConfig (wrapForOrigin origin $ Tasks (OnTasks (Delete tid)))

resourceEdit :: ResourceId -> EditConfig
resourceEdit rid = EditConfig (ResourceLock rid) (Resources (OnResources (Modify rid Lock)))

resourceDelete :: ResourceId -> DeleteConfig
resourceDelete rid = DeleteConfig (Resources (OnResources (Delete rid)))

lessonNotesEdit :: LessonNotesId -> EditConfig
lessonNotesEdit lnid = EditConfig (LessonNotesLock lnid) (Cmd.LessonNotes (OnLessonNotes (Modify lnid Lock)))

lessonNotesDelete :: LessonNotesId -> DeleteConfig
lessonNotesDelete lnid = DeleteConfig (Cmd.LessonNotes (OnLessonNotes (Delete lnid)))

assignmentEdit :: AssignmentId -> EntityOrigin -> EditConfig
assignmentEdit aid origin = EditConfig (AssignmentLock aid) (wrapForOrigin origin $ Assignments (OnAssignments (Modify aid Lock)))

assignmentDelete :: AssignmentId -> EntityOrigin -> DeleteConfig
assignmentDelete aid origin = DeleteConfig (wrapForOrigin origin $ Assignments (OnAssignments (Delete aid)))

lessonEdit :: LessonId -> EditConfig
lessonEdit lid = EditConfig (LessonLock lid) (Lessons (OnLessons (Modify lid Lock)))

lessonDelete :: LessonId -> DeleteConfig
lessonDelete lid = DeleteConfig (Lessons (OnLessons (Delete lid)))

-- ============================================================================
-- Component internals
-- ============================================================================

data MenuModel = MenuModel
  { isOpen :: !Bool
  , lockState :: !LB.LockState
  , holdState :: !(HoldButton.HoldState ())
  }
  deriving (Eq, Show, Generic)

data MenuAction
  = Toggle
  | Close
  | LockAction !LB.LockAction
  | DeleteHold !(HoldButton.HoldAction ())
  | DoPinViewer
  | DoGoTo
  | DoExtraEntry !Int
  deriving (Eq, Show)

entityMenuComponent :: SyncContext -> EntityMenuConfig -> M.Component p MenuModel MenuAction
entityMenuComponent r cfg =
  (M.component model update view)
    { M.subs = lockSubs
    }
  where
    lockFrag = case cfg.edit of
      Just ec -> Just (LB.lockFragmentDef r (LB.LockButtonConfig ec.lock ec.lockCommand Button.regularButtonSize))
      Nothing -> Nothing

    lockSubs = case lockFrag of
      Just frag -> map (mapSub LockAction) frag.subs
      Nothing -> []

    model = MenuModel
      { isOpen = False
      , lockState = maybe LB.initialLockState (.initialModel) lockFrag
      , holdState = HoldButton.emptyHoldState
      }

    update Toggle = M.modify $ \m -> m{isOpen = not m.isOpen}
    update Close = M.modify $ \m -> m{isOpen = False}

    update (LockAction la) = case lockFrag of
      Just frag -> do
        liftEffect_ #lockState LockAction (frag.update la)
        case la of
          LB.Click -> M.modify $ \m -> m{isOpen = False}
          _ -> pure ()
      Nothing -> pure ()

    update (DeleteHold ha) = case cfg.delete of
      Just dc -> do
        executed <- liftEffect #holdState DeleteHold $
          HoldButton.updateHold (\() -> modifySyncDocument r dc.deleteCommand) ha
        when executed $ M.modify $ \m -> m{isOpen = False}
      Nothing -> pure ()

    update DoPinViewer = do
      M.modify $ \m -> m{isOpen = False}
      case cfg.pin of
        Just req -> M.io_ $ requestViewerPin r req
        Nothing -> pure ()

    update DoGoTo = do
      M.modify $ \m -> m{isOpen = False}
      case cfg.goTo of
        Just page -> M.io_ $ M.pushURI (M.toURI page)
        Nothing -> pure ()

    update (DoExtraEntry idx) = do
      M.modify $ \m -> m{isOpen = False}
      case drop idx cfg.extraEntries of
        (entry : _) -> M.io_ entry.action
        [] -> pure ()

    view :: MenuModel -> M.View MenuModel MenuAction
    view m =
      let trigger =
            MH.span_
              [class_ "inline-flex items-center justify-center p-1.5 rounded-md text-muted-foreground hover:bg-muted hover:text-foreground transition-colors cursor-pointer"]
              [Icon.iconS Icon.Small Icon.IcnMenu]
          items = concat
            [ editEntry m
            , [menuButton Icon.IcnPin (C.translate' C.LblPin) DoPinViewer | Just _ <- [cfg.pin]]
            , [menuButton Icon.IcnOpenModal (C.translate' C.LblGoTo) DoGoTo | Just _ <- [cfg.goTo]]
            , map (\(i, e) -> menuButton e.icon e.label (DoExtraEntry i)) (zip [0 ..] cfg.extraEntries)
            , deleteEntry m
            ]
       in HoverMenu.clickMenuRight m.isOpen Toggle Close trigger items

    editEntry m = case cfg.edit of
      Nothing -> []
      Just _ ->
        [LB.lockView menuSize m.lockState LockAction]

    deleteEntry m = case cfg.delete of
      Nothing -> []
      Just _ ->
        [ HoverMenu.hoverMenuSeparator
        , HoldButton.holdButton DeleteHold m.holdState () Button.Destructive menuSize
            (Button.IconText Icon.IcnDelete (C.translate' C.LblDelete))
        ]

    menuSize = Button.ButtonSize Button.Small Button.Full

    menuButton icn label action =
      Button.render Button.Ghost menuSize (Button.ButtonConfig (Button.IconText icn label) (Just action))


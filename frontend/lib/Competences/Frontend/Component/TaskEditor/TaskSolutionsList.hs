module Competences.Frontend.Component.TaskEditor.TaskSolutionsList
  ( taskSolutionsListComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), SolutionsCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Solution (..), User (..))
import Competences.Document.Solution (SolutionId, SolutionIxs, SolutionType (..), mkSolution)
import Competences.Document.Task (TaskId)
import Competences.Document.User (isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.Frontend.Component.TaskEditor.SolutionEditorDetail (solutionInlineEditor)
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , SyncDocumentEnv (..)
  , mkCreateAndLock
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  , syncDocumentEnv
  )
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (.~))

-- ============================================================================
-- Projection
-- ============================================================================

-- | Projection type for the solutions list
data Projection = Projection
  { solutions :: !(Ix.IxSet SolutionIxs Solution)
  -- ^ Solutions for this task only
  , connectedUser :: !User
  }
  deriving (Eq, Generic, Show)

emptyProjection :: User -> Projection
emptyProjection = Projection Ix.empty

-- | Projection function - filters solutions by task ID
solutionsProjection :: User -> TaskId -> Document -> Maybe User -> Projection
solutionsProjection connectedUser taskId doc _mUser =
  Projection
    { solutions = doc.solutions Ix.@= taskId
    , connectedUser = connectedUser
    }

-- ============================================================================
-- Model and Actions
-- ============================================================================

data Model = Model
  { projection :: !Projection
  , taskId :: !TaskId
  , expandedSolutions :: !(Set SolutionId)
  -- ^ Which solutions are expanded to show full content
  }
  deriving (Eq, Generic, Show)

data Action
  = ProjectionChanged !(ProjectedChange Projection)
  | ToggleSolution !SolutionId
  | CreateSolution
  | DeleteSolution !SolutionId
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Component showing solutions for a specific task
-- Teachers can add/delete solutions
-- Everyone can view and expand/collapse solutions
taskSolutionsListComponent
  :: SyncContext
  -> TaskId
  -> M.Component p Model Action
taskSolutionsListComponent r taskId =
  (M.component model update view')
    { M.subs = [subscribeWithProjection r (solutionsProjection connectedUser taskId) ProjectionChanged]
    }
  where
    env = syncDocumentEnv r
    connectedUser = env.connectedUser

    model =
      Model
        { projection = emptyProjection connectedUser
        , taskId = taskId
        , expandedSolutions = Set.empty
        }

    update (ProjectionChanged change) =
      M.modify $ #projection .~ change.projection
    update (ToggleSolution solId) =
      M.modify $ \m ->
        m
          & #expandedSolutions
          .~ ( if Set.member solId m.expandedSolutions
                 then Set.delete solId m.expandedSolutions
                 else Set.insert solId m.expandedSolutions
             )
    update CreateSolution = M.withSink $ \_sink -> do
      solutionId <- nextId r
      let newSolution = mkSolution solutionId taskId connectedUser.id
      modifySyncDocument r $ Solutions (OnSolutions (mkCreateAndLock r newSolution))
    update (DeleteSolution solId) = M.withSink $ \_sink -> do
      modifySyncDocument r $ Solutions (OnSolutions (Delete solId))

    view' m =
      Card.card
        [ MH.div_
            [class_ "space-y-4"]
            [ viewHeader m
            , viewSolutionsList m
            ]
        ]

    -- Header with title and add button
    viewHeader m =
      Layout.hFlow
        (Layout.hFull <> Layout.crossCenter <> Layout.mainBetween)
        [ Typography.h3 $ C.translate' C.LblSolutions
        , if isTeacher m.projection.connectedUser
            then
              Button.secondarySm (Button.button (Icon.IcnAdd, C.LblAddSolution) CreateSolution)
            else Layout.empty
        ]

    -- List of solutions
    viewSolutionsList m =
      let sols = Ix.toList m.projection.solutions
       in if null sols
            then
              MH.div_
                [class_ "text-muted-foreground text-sm py-4 text-center"]
                [M.text $ C.translate' C.LblNoSolutions]
            else
              MH.div_
                [class_ "space-y-2"]
                (map (viewSolution m) sols)

    -- Single solution item
    viewSolution m sol =
      let isExpanded = Set.member sol.id m.expandedSolutions
          isOwner = isTeacher m.projection.connectedUser
          titleView = Disclosure.titleIconText Icon.IcnSolution (solutionTypeLabel sol.solutionType)
          bodyView =
            if isOwner
              then solutionInlineEditor r sol
              else
                if sol.content == mempty
                  then Typography.muted "Kein Inhalt"
                  else
                    MH.div_
                      [class_ "prose prose-stone prose-sm max-w-none"]
                      [renderRichText r.formulaCache sol.content]
       in Disclosure.disclosure (ToggleSolution sol.id) $
            Disclosure.contents titleView isExpanded bodyView
              [Disclosure.destructiveAction Icon.IcnDelete (DeleteSolution sol.id) | isOwner]

    solutionTypeLabel :: SolutionType -> M.MisoString
    solutionTypeLabel = C.translate' . C.LblSolutionType

module Competences.Frontend.Component.Solution.ViewerDetail
  ( viewerDetailView
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , Solution (..)
  , Task (..)
  , User (..)
  )
import Competences.Document.Solution (SolutionType (..))
import Competences.Document.Task (TaskIdentifier (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.Component.TaskContentView (renderRichText)
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , subscribeWithProjection
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (ms)
import Optics.Core ((.~))

-- ============================================================================
-- Viewer Projection (pre-computed data)
-- ============================================================================

-- | Pre-computed projection for the viewer
data ViewerProjection = ViewerProjection
  { -- | The current solution (may be updated if edited elsewhere)
    currentSolution :: !Solution
    -- | Task name (looked up from document)
  , taskName :: !M.MisoString
    -- | Author name (looked up from document)
  , authorName :: !M.MisoString
  }
  deriving (Eq, Generic, Show)

-- | Empty projection for initial state
emptyProjection :: Solution -> ViewerProjection
emptyProjection solution = ViewerProjection
  { currentSolution = solution
  , taskName = ""
  , authorName = ""
  }

-- ============================================================================
-- Viewer Detail Component
-- ============================================================================

-- | Detail view for viewing a solution (read-only)
-- Shows solution details and renders content with MathJax
viewerDetailView
  :: SyncContext
  -> Solution
  -> M.View (SD.Model Solution mode) (SD.Action mode)
viewerDetailView r solution =
  V.component
    ("solution-viewer-" <> M.ms (show solution.id))
    (viewerComponent r solution)

-- | Minimal model - only stores the projection
data ViewerModel = ViewerModel
  { projection :: !ViewerProjection
  }
  deriving (Eq, Generic, Show)

data ViewerAction
  = ProjectionChanged !(ProjectedChange ViewerProjection)
  deriving (Eq, Show)

-- | The viewer component using subscribeWithProjection pattern
viewerComponent :: SyncContext -> Solution -> M.Component p ViewerModel ViewerAction
viewerComponent r solution =
  (M.component model update view')
    { M.subs = [subscribeWithProjection r (viewerProjection solution) ProjectionChanged]
    }
  where
    model = ViewerModel
      { projection = emptyProjection solution
      }

    -- Projection function captures solution from closure
    viewerProjection :: Solution -> Document -> Maybe User -> ViewerProjection
    viewerProjection sol doc _mUser =
      let -- Look up the current solution from the document (in case it was edited)
          updatedSolution = maybe sol id $ Ix.getOne (doc.solutions Ix.@= sol.id)

          -- Look up task name
          taskName = case Ix.getOne (doc.tasks Ix.@= updatedSolution.taskId) of
            Nothing -> "(Aufgabe nicht gefunden)"
            Just task ->
              let TaskIdentifier ident = task.identifier
               in ms ident

          -- Look up author name
          authorName = case Ix.getOne (doc.users Ix.@= updatedSolution.userId) of
            Nothing -> "(Unbekannt)"
            Just user -> ms user.name

       in ViewerProjection
            { currentSolution = updatedSolution
            , taskName = taskName
            , authorName = authorName
            }

    update (ProjectionChanged change) =
      M.modify $ #projection .~ change.projection

    view' m =
      M.div_
        [class_ "space-y-6"]
        [ viewSolutionHeader m
        , viewSolutionContent m
        ]

    viewSolutionHeader m =
      let proj = m.projection
          sol = proj.currentSolution
       in Card.card
            [ M.div_
                [class_ "space-y-2"]
                [ -- Line 1: Task identifier
                  Typography.h2 proj.taskName
                , -- Line 2: Solution type badge + Author
                  M.div_
                    [class_ "flex items-center gap-4 text-sm"]
                    [ Badge.badge (solutionTypeBadgeVariant sol.solutionType) (solutionTypeLabel sol.solutionType)
                    , M.span_
                        [class_ "text-muted-foreground"]
                        [M.text $ "von " <> proj.authorName]
                    ]
                ]
            ]

    viewSolutionContent m =
      let sol = m.projection.currentSolution
       in Card.card
            [ M.div_
                [class_ "space-y-2"]
                [ Typography.h3 $ C.translate' C.LblSolutionContent
                , if sol.content == ""
                    then Typography.muted "Kein Inhalt"
                    else M.div_
                           [class_ "prose prose-stone max-w-none"]
                           [renderRichText sol.content]
                ]
            ]

    solutionTypeLabel :: SolutionType -> M.MisoString
    solutionTypeLabel = C.translate' . C.LblSolutionType

    solutionTypeBadgeVariant :: SolutionType -> Badge.BadgeVariant
    solutionTypeBadgeVariant Hint = Badge.BadgeSecondary
    solutionTypeBadgeVariant Results = Badge.BadgeOutline
    solutionTypeBadgeVariant Complete = Badge.BadgePrimary

module Competences.Frontend.Component.Selector.SolutionSelector
  ( solutionSelectorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), SolutionsCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Solution (..), Task (..), User (..))
import Competences.Document.Solution (SolutionIxs, SolutionType (..), mkSolution)
import Competences.Document.Task (TaskId, TaskIdentifier (..))
import Competences.Document.User (UserId, isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , SyncDocumentEnv (..)
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  , syncDocumentEnv
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Frontend.View.Tailwind (class_)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString, ms)
import Optics.Core (Lens', toLensVL, (&), (.~), (?~), (^.))

-- | Projection from document for efficient rendering
data SelectorProjection = SelectorProjection
  { solutions :: !(Ix.IxSet SolutionIxs Solution)
  , tasks :: !(Map.Map TaskId Task)
  , users :: !(Map.Map UserId User)
  , connectedUser :: !User
  }
  deriving (Eq, Generic, Show)

emptyProjection :: User -> SelectorProjection
emptyProjection user = SelectorProjection Ix.empty Map.empty Map.empty user

-- | Projection function
selectorProjection :: User -> Document -> Maybe User -> SelectorProjection
selectorProjection connectedUser doc _mUser =
  let sols = doc.solutions
      -- Build lookup maps for tasks and users referenced by solutions
      taskIds = map (.taskId) $ Ix.toList sols
      userIds = map (.userId) $ Ix.toList sols
      taskMap = Map.fromList [(t.id, t) | t <- Ix.toList $ doc.tasks Ix.@+ taskIds]
      userMap = Map.fromList [(u.id, u) | u <- Ix.toList $ doc.users Ix.@+ userIds]
   in SelectorProjection
        { solutions = sols
        , tasks = taskMap
        , users = userMap
        , connectedUser = connectedUser
        }

data Model = Model
  { projection :: !SelectorProjection
  , selectedSolution :: !(Maybe Solution) -- bound to parent
  , newSolution :: !(Maybe Solution) -- temporary for new solutions
  , searchQuery :: !Text
  , filterAuthor :: !(Maybe UserId)
  , filterType :: !(Maybe SolutionType)
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectSolution !Solution
  | CreateNewSolution
  | SetSearchQuery !Text
  | SetFilterAuthor !(Maybe UserId)
  | SetFilterType !(Maybe SolutionType)
  | ProjectionChanged !(ProjectedChange SelectorProjection)
  deriving (Eq, Show)

solutionSelectorComponent
  :: SyncContext -> Lens' p (Maybe Solution) -> M.Component p Model Action
solutionSelectorComponent r parentLens =
  (M.component model update view')
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedSolution]
    , M.subs = [subscribeWithProjection r (selectorProjection connectedUser) ProjectionChanged]
    }
  where
    env = syncDocumentEnv r
    connectedUser = env ^. #connectedUser

    model =
      Model
        { projection = emptyProjection connectedUser
        , selectedSolution = Nothing
        , newSolution = Nothing
        , searchQuery = ""
        , filterAuthor = Nothing
        , filterType = Nothing
        }

    update (SelectSolution s) =
      M.modify $ \m -> case Ix.getOne (m.projection.solutions Ix.@= s.id) of
        Just s' -> m & (#selectedSolution ?~ s') & (#newSolution .~ Nothing)
        Nothing -> m & (#newSolution ?~ s)

    update CreateNewSolution = M.withSink $ \sink -> do
      solutionId <- nextId r
      -- We need a task ID - for now create with a placeholder that must be selected in editor
      -- This will be improved when we add task selection to the editor
      taskId <- nextId r
      let newSolution = mkSolution solutionId taskId connectedUser.id
      modifySyncDocument r $ Solutions (OnSolutions (CreateAndLock newSolution))
      sink (SelectSolution newSolution)

    update (SetSearchQuery q) = M.modify $ #searchQuery .~ q

    update (SetFilterAuthor mUserId) = M.modify $ #filterAuthor .~ mUserId

    update (SetFilterType mType) = M.modify $ #filterType .~ mType

    update (ProjectionChanged change) =
      M.modify $ #projection .~ change.projection

    view' m =
      V.viewFlow
        ( V.vFlow
            & (#gap .~ V.SmallSpace)
            & (#expandDirection .~ V.Expand V.Start)
            & (#extraAttrs .~ [V.fullHeight])
        )
        [ SL.selectorHeader (C.translate' C.LblSolutions) createButton
        , SL.selectorSearchField (ms m.searchQuery) (C.translate' C.LblFilterSolutions) (SetSearchQuery . M.fromMisoString)
        , viewFilters m
        , SL.selectorList (map (viewSolution m) (filteredSolutions m))
        ]

    -- Only teachers can create solutions
    createButton =
      if isTeacher connectedUser
        then Just CreateNewSolution
        else Nothing

    viewFilters m =
      M.div_
        [class_ "flex gap-2 flex-wrap"]
        [ viewTypeFilter m
        , viewAuthorFilter m
        ]

    viewTypeFilter m =
      M.div_
        [class_ "flex gap-1"]
        ( typeFilterButton m Nothing "Alle"
            : map (\t -> typeFilterButton m (Just t) (solutionTypeLabel t)) [Hint, Results, Complete]
        )

    typeFilterButton m filterValue label =
      let isActive = m.filterType == filterValue
          baseClass = "px-2 py-1 text-xs rounded-full cursor-pointer transition-colors "
          activeClass = if isActive then "bg-primary text-primary-foreground" else "bg-muted hover:bg-muted/80"
       in M.button_
            [ class_ (baseClass <> activeClass)
            , M.onClick (SetFilterType filterValue)
            ]
            [M.text label]

    viewAuthorFilter m =
      let authors = Map.elems m.projection.users
       in if null authors
            then M.text ""
            else
              M.div_
                [class_ "flex gap-1"]
                ( authorFilterButton m Nothing "Alle Autoren"
                    : map (\u -> authorFilterButton m (Just u.id) (ms u.name)) authors
                )

    authorFilterButton m filterValue label =
      let isActive = m.filterAuthor == filterValue
          baseClass = "px-2 py-1 text-xs rounded-full cursor-pointer transition-colors "
          activeClass = if isActive then "bg-secondary text-secondary-foreground" else "bg-muted hover:bg-muted/80"
       in M.button_
            [ class_ (baseClass <> activeClass)
            , M.onClick (SetFilterAuthor filterValue)
            ]
            [M.text label]

    filteredSolutions m =
      let proj = m.projection
          query = T.toLower m.searchQuery
          allSolutions = Ix.toList proj.solutions
          -- Apply type filter
          typeFiltered = case m.filterType of
            Nothing -> allSolutions
            Just t -> filter (\s -> s.solutionType == t) allSolutions
          -- Apply author filter
          authorFiltered = case m.filterAuthor of
            Nothing -> typeFiltered
            Just uid -> filter (\s -> s.userId == uid) typeFiltered
          -- Apply text search (on task identifier)
          textFiltered =
            if T.null query
              then authorFiltered
              else
                filter
                  ( \s -> case Map.lookup s.taskId proj.tasks of
                      Nothing -> False
                      Just task ->
                        let TaskIdentifier ident = task.identifier
                         in query `T.isInfixOf` T.toLower ident
                  )
                  authorFiltered
       in textFiltered

    viewSolution m s =
      let proj = m.projection
          isSelected = m.selectedSolution == Just s || m.newSolution == Just s
          taskLabel = case Map.lookup s.taskId proj.tasks of
            Nothing -> "(Aufgabe nicht gefunden)"
            Just task -> let TaskIdentifier ident = task.identifier in ms ident
          authorLabel = case Map.lookup s.userId proj.users of
            Nothing -> ""
            Just user -> " - " <> ms user.name
          badge = Badge.badge (solutionTypeBadgeVariant s.solutionType) (solutionTypeLabel s.solutionType)
       in SL.selectorItemWithBadge isSelected V.IcnSolution (taskLabel <> authorLabel) (Just badge) (SelectSolution s)

    solutionTypeLabel :: SolutionType -> MisoString
    solutionTypeLabel = C.translate' . C.LblSolutionType

    solutionTypeBadgeVariant :: SolutionType -> Badge.BadgeVariant
    solutionTypeBadgeVariant Hint = Badge.BadgeSecondary
    solutionTypeBadgeVariant Results = Badge.BadgeOutline
    solutionTypeBadgeVariant Complete = Badge.BadgePrimary

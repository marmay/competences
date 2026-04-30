-- | Task entity selector — a thin config wrapper around
-- 'entitySelectorComponent'.
--
-- Builds the projection (drafts + published merged into a single
-- @IxSet TaskWithOriginIxs (WithOrigin Task)@), the create dropdown
-- entries (published / draft), the URL binding, and the search
-- filter. The generic body owns selection state, URL synchronisation,
-- and the create-then-promote workflow.
{-# OPTIONS_GHC -Wno-orphans #-}
module Competences.Frontend.Component.Task.Selector
  ( TaskWithOriginIxs
  , Selected
  , Projection
  , taskSelectorComponent
  )
where

import Competences.Command (Command (..), DraftTasksCommand (..), EntityCommand (..), TasksCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Task (..))
import Competences.Document.Task (TaskId, TaskIdentifier, defaultTask, taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Common.WithOrigin (WithOrigin (..))
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.Component.Selector.Entity
  ( Action (..)
  , CreateAction (..)
  , EntitySelectorConfig (..)
  , ItemRenderer (..)
  , Model
  , entitySelectorComponent
  )
import Competences.Frontend.Component.Selector.UriBinding (pageBinding)
import Competences.Frontend.Fragment.SelectorFilter (searchOnlyFilter)
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext (SyncContext, modifySyncDocument, nextId)
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.SelectorList qualified as SL
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Miso qualified as M
import Miso.String (ms)
import Optics.Core (Lens')

-- | The selector's selected type: a task tagged with its origin.
type Selected = WithOrigin Task

-- | Indices for the merged published+draft IxSet.
type TaskWithOriginIxs = '[TaskId, TaskIdentifier, EntityOrigin]

-- | Projection alias used in the generic selector's type parameters.
type Projection = Ix.IxSet TaskWithOriginIxs Selected

instance Ix.Indexable TaskWithOriginIxs Selected where
  indices =
    Ix.ixList
      (Ix.ixFun $ \w -> [w.value.id])
      (Ix.ixFun $ \w -> [w.value.identifier])
      (Ix.ixFun $ \w -> [w.origin])

-- | Mount a task selector. @parentLens@ points to the
-- @Maybe (WithOrigin Task)@ slot in the parent's model that the
-- selector writes to.
taskSelectorComponent
  :: SyncContext
  -> Maybe TaskId
  -- ^ Deep-linked task at first mount, if any.
  -> Lens' p (Maybe Selected)
  -> M.Component p (Model Selected Projection Text) (Action Selected Projection Text)
taskSelectorComponent r mTaskId parentLens =
  entitySelectorComponent r (config parentLens mTaskId)

config
  :: Lens' p (Maybe Selected)
  -> Maybe TaskId
  -> EntitySelectorConfig p Selected Projection TaskWithOriginIxs TaskId Text Text
config parentLens mTaskId =
  EntitySelectorConfig
    { title = C.translate' C.LblTasks
    , project = \doc _user -> projectTasks doc
    , entitiesOf = id
    , itemsInOrder = Ix.toAscList (Proxy @TaskIdentifier)
    , idOf = (.value.id)
    , lookupBy = \xs tid -> Ix.getOne (xs Ix.@= tid)
    , itemView = ItemRenderer renderItem
    , createActions =
        [ CreateAction
            { icon = Icon.IcnTask
            , label = C.translate' C.LblNewTask
            , run = \r -> do
                tid <- nextId r
                let t = defaultTask tid
                modifySyncDocument r $ Tasks (OnTasks (CreateAndLock t))
                pure (WithOrigin Published t)
            }
        , CreateAction
            { icon = Icon.IcnTask
            , label = C.translate' C.LblNewDraftTask
            , run = \r -> do
                tid <- nextId r
                let t = defaultTask tid
                modifySyncDocument r $ DraftTasks (OnDraftTasks (CreateAndLock t))
                pure (WithOrigin Draft t)
            }
        ]
    , uriBinding =
        Just $ pageBinding (ManageTasks . Just) $ \case
          ManageTasks (Just tid) -> Just tid
          _ -> Nothing
    , initialPick = Just $ \xs ->
        case mTaskId of
          Just tid -> case Ix.getOne (xs Ix.@= tid) of
            Just hit -> Just hit
            Nothing -> firstByIdentifier xs
          Nothing -> firstByIdentifier xs
    , filter = searchOnlyFilter (C.translate' C.LblFilterTasks) (taskDisplayName . (.value))
    , parentLens = parentLens
    }

renderItem
  :: Selected
  -> Bool
  -> M.View m (Action Selected Projection Text)
renderItem wt isSel =
  let badge = case wt.origin of
        Draft -> Just $ Badge.secondary (Badge.badgeText (C.translate' C.LblDraft))
        Published -> Nothing
   in SL.selectorItemWithBadge isSel Icon.IcnTask (ms (taskDisplayName wt.value)) badge (Pick wt)

firstByIdentifier :: Projection -> Maybe Selected
firstByIdentifier xs = case Ix.toAscList (Proxy @TaskIdentifier) xs of
  [] -> Nothing
  (x : _) -> Just x

projectTasks :: Document -> Projection
projectTasks doc =
  Ix.fromList $
    map (WithOrigin Published) (Ix.toList doc.tasks)
      <> map (WithOrigin Draft) (Ix.toList doc.draftTasks)

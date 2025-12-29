module Competences.Frontend.Component.Selector.SelfContainedTaskSelector
  ( selfContainedTaskSelectorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), TasksCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Task (..), TaskIxs, TaskType (..))
import Competences.Document.Task (TaskIdentifier (..), defaultTaskAttributes)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Data.List (sortOn)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString, ms)
import Optics.Core (Lens', toLensVL, (&), (.~), (?~))

data Model = Model
  { allTasks :: !(Ix.IxSet TaskIxs Task)
  , selectedTask :: !(Maybe Task)
  , newTask :: !(Maybe Task)
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectTask !Task
  | CreateNewTask
  | UpdateDocument !DocumentChange
  deriving (Eq, Show)

selfContainedTaskSelectorComponent
  :: SyncDocumentRef -> Lens' p (Maybe Task) -> M.Component p Model Action
selfContainedTaskSelectorComponent r parentLens =
  (M.component model update view)
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedTask]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = Model Ix.empty Nothing Nothing

    update (SelectTask t) =
      M.modify $ \m -> case Ix.getOne (m.allTasks Ix.@= t.id) of
        Just t' -> m & (#selectedTask ?~ t') & (#newTask .~ Nothing)
        Nothing -> m & (#newTask ?~ t)

    update CreateNewTask = M.withSink $ \s -> do
      taskId <- nextId r
      let newTask = Task
            { id = taskId
            , identifier = TaskIdentifier ""
            , content = Nothing
            , taskType = SelfContained defaultTaskAttributes
            }
      -- Emit command to create the task
      modifySyncDocument r $ Tasks (OnTasks (Create newTask))
      -- Select the newly created task
      s (SelectTask newTask)

    update (UpdateDocument dc) = M.modify $ \m ->
      let allSelfContainedTasks = Ix.fromList $
            filter isSelfContained $ Ix.toList dc.document.tasks
       in m & #allTasks .~ allSelfContainedTasks

    view m =
      M.div_
        []
        [ M.h2_ [] [M.text $ C.translate' C.LblSelfContainedTasks]
        , M.button_
            [M.onClick CreateNewTask]
            [M.text $ C.translate' C.LblNewTask]
        , M.div_
            []
            (map viewTaskItem (sortedTasks m))
        ]

    sortedTasks m = sortOn (\t -> t.identifier) $ Ix.toList m.allTasks

    viewTaskItem task =
      M.div_
        [M.onClick (SelectTask task)]
        [M.text $ taskIdentifierToMiso task.identifier]

    isSelfContained :: Task -> Bool
    isSelfContained task = case task.taskType of
      SelfContained _ -> True
      SubTask _ _ -> False

    taskIdentifierToMiso :: TaskIdentifier -> MisoString
    taskIdentifierToMiso (TaskIdentifier t) = ms t

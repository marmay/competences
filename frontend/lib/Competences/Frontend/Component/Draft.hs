module Competences.Frontend.Component.Draft
  ( EntityOrigin (..)
  , isDraft
  , retargetForDraft
  , wrapForOrigin
  )
where

import Competences.Command
  ( AssignmentsCommand (..)
  , Command (..)
  , DraftAssignmentsCommand (..)
  , DraftTasksCommand (..)
  , TasksCommand (..)
  )

-- | Whether an entity comes from the published or draft collection
data EntityOrigin = Published | Draft
  deriving (Eq, Show)

isDraft :: EntityOrigin -> Bool
isDraft Draft = True
isDraft Published = False

-- | Retarget a command from real collections to draft collections
retargetForDraft :: Command -> Command
retargetForDraft (Tasks (OnTasks cmd)) = DraftTasks (OnDraftTasks cmd)
retargetForDraft (Assignments (OnAssignments cmd)) = DraftAssignments (OnDraftAssignments cmd)
retargetForDraft cmd = cmd

-- | Get the appropriate command wrapper for an entity origin
wrapForOrigin :: EntityOrigin -> Command -> Command
wrapForOrigin Published = id
wrapForOrigin Draft = retargetForDraft

# Task System Design

## Overview

Tasks are a fundamental building block for tracking student work, providing practice resources, and evaluating competences. They address three key requirements:

1. **Track student workload**: Know what tasks students are completing
2. **Suggest practice resources**: Show students which tasks help develop specific competences
3. **Derive competence coverage**: Automatically determine which competences are tested by a task

## Design Goals

- **Rich metadata**: Tasks carry information about which competences they test (primary and secondary)
- **Flexibility**: Support both standalone tasks and grouped task sequences
- **Simplicity**: Clear reference semantics, no deep nesting
- **User-friendly**: Human-readable identifiers for looking up tasks

## Data Model

### TaskPurpose

Distinguishes between practice and assessment tasks:

```haskell
data TaskPurpose = Practice | Assessment
  deriving (Eq, Show, Generic, Ord)
```

- **Practice**: Helps develop competence, but not sufficient alone to prove achievement
- **Assessment**: Clearly demonstrates competence has been achieved
- Practice tasks can be used in Evidence when multiple tasks or oral explanation provides proof

### TaskAttributes

Core attributes that define what a task tests and how it's displayed:

```haskell
data TaskAttributes = TaskAttributes
  { primary :: [CompetenceLevelId]
    -- ^ Competences that this task primarily tests for
  , secondary :: [CompetenceLevelId]
    -- ^ Competences that may be tested by this task
  , purpose :: TaskPurpose
    -- ^ Practice (develops competence) or Assessment (proves competence)
  , displayInResources :: Bool
    -- ^ Whether to show this task in resource view for its primary competences
    -- Separate from purpose: controls timing (e.g., hide exam until after test date)
  }
```

### TaskAttributesOverride

Used by subtasks to selectively override group defaults:

```haskell
data TaskAttributesOverride = TaskAttributesOverride
  { primary :: Maybe [CompetenceLevelId]      -- Nothing = inherit from group
  , secondary :: Maybe [CompetenceLevelId]    -- Nothing = inherit from group
  , purpose :: Maybe TaskPurpose              -- Nothing = inherit from group
  , displayInResources :: Maybe Bool          -- Nothing = inherit from group
  }
```

**Inheritance semantics:**
- `Nothing`: Inherit attribute value from TaskGroup's defaultTaskAttributes
- `Just x`: Use value `x`, override group default

### TaskGroup

Organizational unit for related tasks with shared defaults:

```haskell
data TaskGroup = TaskGroup
  { id :: TaskGroupId
  , identifier :: Text
    -- ^ Human-readable identifier (e.g., "Book-Chapter-1.2")
    -- User is responsible for uniqueness
  , defaultTaskAttributes :: TaskAttributes
    -- ^ Default attributes inherited by subtasks
  , contentBefore :: Maybe Text
    -- ^ Content displayed before subtask content (when rendering)
  , contentAfter :: Maybe Text
    -- ^ Content displayed after subtask content (when rendering)
  }
```

**Notes:**
- TaskGroup does NOT store a list of member tasks
- Member tasks are queried from Document's Task IxSet (indexed by TaskGroupId)
- Task ordering within group: lexical sort by task identifier

### Task

Atomic unit of work, either standalone or part of a group:

```haskell
data Task = Task
  { id :: TaskId
  , identifier :: Text
    -- ^ Human-readable identifier (e.g., "Book-1.2.3.a", "Worksheet-15-Task-2")
    -- User is responsible for uniqueness
  , content :: Maybe Text
    -- ^ Inline task content (if provided)
    -- Nothing = reference-only task (students look up by identifier)
    -- Just text = task content shown inline
    -- Future: will support markup language
  , taskType :: TaskType
  }

data TaskType
  = SelfContained TaskAttributes
    -- ^ Standalone task with own complete attributes
  | SubTask TaskGroupId TaskAttributesOverride
    -- ^ Task belonging to a group, inheriting defaults with optional overrides
```

**Constraints:**
- A task can belong to at most one TaskGroup (enforced by SubTask constructor)
- Identifier is user-managed, not enforced unique (allows flexible naming schemes)

### Content Rendering

When displaying a SubTask, content is composed as:

```
[TaskGroup.contentBefore]
[Task.content]
[TaskGroup.contentAfter]
```

Example:
```
TaskGroup "Worksheet 5"
  contentBefore = "Solve the following equations:"
  contentAfter = "Show your work step by step."

Task "Worksheet-5.a"
  content = "3x + 5 = 17"
  taskType = SubTask "Worksheet 5" {...}

Rendered output:
  Solve the following equations:
  3x + 5 = 17
  Show your work step by step.
```

## Integration with Existing Entities

### Evidence

Evidence references specific tasks that were completed:

```haskell
-- Old (current):
data Evidence = Evidence
  { ...
  , activityTasks :: ActivityTasks  -- ActivityTasks wraps Text
  , ...
  }

-- New (with gradual migration support):
data Evidence = Evidence
  { ...
  , tasks :: [TaskId]           -- New: task references
  , oldTasks :: Maybe Text      -- Old: legacy text-based tasks
  , ...
  }
```

**Migration Strategy:**
- Add `oldTasks :: Maybe Text` field for backward compatibility
- Keep existing data in `oldTasks` field
- New evidences use `tasks :: [TaskId]`
- Gradual migration: user can manually convert old evidences to new format over time
- Display logic: show `oldTasks` if `tasks` is empty, otherwise show resolved task names from `tasks`

**Note:** Due to historical migrations, existing task data is inconsistent and will require manual cleanup during gradual migration.

### Assignment (replaces Template)

**Current state:** Template exists in codebase but is not used productively yet.

**Planned state:** Assignment replaces Template entirely (no coexistence).

**Reasoning for the shift:**
- **Template** was designed as a reusable schema for creating evidences with task-by-task evaluation
- **Assignment** is a more natural concept: concrete assignments given to specific students on specific dates
- Tasks are now rich enough (with competence metadata) to encode all evaluation aspects
- Students should see what's assigned to them → Assignment becomes a student-facing entity
- No need for reusable templates when tasks can be easily selected and grouped

**Assignment structure (to be refined after Task implementation):**

```haskell
data Assignment = Assignment
  { id :: AssignmentId
  , name :: Text                    -- Assignment name (e.g., "Homework Week 5")
  , assignmentDate :: Day           -- When assigned (controls visibility to students)
  , dueDate :: Maybe Day            -- Optional deadline
  , userIds :: Set UserId           -- Assigned to these students
  , tasks :: [TaskId]               -- Tasks to complete
  , activityType :: ActivityType    -- Type of activity
  , socialForm :: SocialForm        -- Individual, pair work, group work, etc.
  }

data AssignmentEvaluation = AssignmentEvaluation
  { assignment :: Assignment
  , assessments :: [(TaskId, Maybe Ability)]  -- Teacher's assessment per task
  }
```

**Assignment lifecycle:**
1. **Creation**: Teacher creates assignment (date, students, tasks)
2. **Visibility**: Students see assignment in their projected view (filtered by assignmentDate for tests)
3. **Evaluation**: Teacher uses AssignmentEvaluation (ephemeral UI state) to assess task-by-task
4. **Finalization**: Creates Evidence from aggregated assessments

**Phased implementation:**
- **Phase 1**: Implement Tasks (this design document)
- **Phase 2**: Implement Assignment as evaluation tool (students don't see yet)
- **Phase 3**: UI polish → first semi-productive release
- **Phase 4**: Enable student visibility of Tasks and Assignments
- **Phase 5**: Markup language for inline task content

### CompetenceGrid Resources

When clicking on a CompetenceGrid cell, show tasks available for practice:

- Filter tasks where `displayInResources = True`
- Filter by `CompetenceLevelId ∈ task.primary`
- Display as practice suggestions

**Current Resource entity:**
- Resource has URL and description (for external links)
- Decision: Keep Resource for external links, use Task for internal exercises
- Tasks complement Resources, not replace them

## IxSet Indices

Tasks need to be indexed by:
- TaskId (primary lookup)
- Identifier (search by human-readable name)
- TaskGroupId (for SubTask queries, to find all tasks in a group)

```haskell
type TaskIxs = '[TaskId, Text, Maybe TaskGroupId]

instance Ix.Indexable TaskIxs Task where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.identifier))
      (Ix.ixFun $ singleton . taskGroupId)
    where
      taskGroupId :: Task -> Maybe TaskGroupId
      taskGroupId task = case task.taskType of
        SelfContained _ -> Nothing
        SubTask gid _ -> Just gid
```

## Common Accessor Functions

To work with tasks uniformly regardless of whether they're SelfContained or SubTask, provide helper functions that abstract over TaskType:

```haskell
-- Get resolved attributes for any task
getTaskAttributes :: Document -> Task -> TaskAttributes
getTaskAttributes doc task = case task.taskType of
  SelfContained attrs -> attrs
  SubTask groupId override ->
    case Ix.getOne (Ix.getEQ groupId doc.taskGroups) of
      Nothing -> error $ "TaskGroup not found: " <> show groupId
      Just group ->
        let defaults = group.defaultTaskAttributes
        in TaskAttributes
          { primary = fromMaybe defaults.primary override.primary
          , secondary = fromMaybe defaults.secondary override.secondary
          , purpose = fromMaybe defaults.purpose override.purpose
          , displayInResources = fromMaybe defaults.displayInResources override.displayInResources
          }

-- Get composed content for any task
getTaskContent :: Document -> Task -> Maybe Text
getTaskContent doc task = case task.taskType of
  SelfContained _ -> task.content
  SubTask groupId _ ->
    case Ix.getOne (Ix.getEQ groupId doc.taskGroups) of
      Nothing -> task.content  -- fallback if group not found
      Just group ->
        let before = fromMaybe "" group.contentBefore
            content = fromMaybe "" task.content
            after = fromMaybe "" group.contentAfter
            composed = before <> content <> after
        in if T.null composed then Nothing else Just composed

-- Check if task should be displayed in resources
isResourceTask :: Document -> Task -> Bool
isResourceTask doc task = displayInResources (getTaskAttributes doc task)

-- Get primary competences for a task
getTaskPrimaryCompetences :: Document -> Task -> [CompetenceLevelId]
getTaskPrimaryCompetences doc task = primary (getTaskAttributes doc task)

-- Get all competences (primary + secondary) for a task
getTaskAllCompetences :: Document -> Task -> [CompetenceLevelId]
getTaskAllCompetences doc task =
  let attrs = getTaskAttributes doc task
  in attrs.primary <> attrs.secondary
```

**Note:** These functions will be implemented in `common/lib/Competences/Document/Task.hs` and used consistently throughout the codebase for accessing task data.

## Reference Semantics

**Evidence and Template always reference individual Tasks (TaskId), never TaskGroups.**

This keeps semantics simple:
- Clear what work was done (specific tasks, not "whole group")
- Easy to determine which competences were tested (look up Task attributes)
- No ambiguity about completion

**Querying tasks in a group:**
```haskell
getTasksInGroup :: TaskGroupId -> Document -> [Task]
getTasksInGroup groupId doc =
  sortOn (.identifier) $
    Ix.toList $
      Ix.getEQ (Just groupId) doc.tasks
```

## Use Cases

### 1. Creating standalone task

```haskell
task = Task
  { id = generateId
  , identifier = "Book-3.2.5"
  , content = Nothing  -- students look it up in the book
  , taskType = SelfContained $ TaskAttributes
      { primary = [mathLevel2, logicLevel1]
      , secondary = [problemSolvingLevel2]
      , purpose = Assessment
      , displayInResources = True
      }
  }
```

### 2. Creating task group with subtasks

```haskell
-- Create group
group = TaskGroup
  { id = groupId
  , identifier = "Worksheet-15"
  , defaultTaskAttributes = TaskAttributes
      { primary = [algebraLevel2]
      , secondary = []
      , purpose = Practice
      , displayInResources = True
      }
  , contentBefore = Just "Solve the following equations:"
  , contentAfter = Just "Check your answers by substitution."
  }

-- Create subtasks
task1 = Task
  { id = task1Id
  , identifier = "Worksheet-15.a"
  , content = Just "2x + 3 = 11"
  , taskType = SubTask groupId $ TaskAttributesOverride
      { primary = Nothing      -- inherit algebraLevel2
      , secondary = Nothing    -- inherit []
      , purpose = Nothing      -- inherit Practice
      , displayInResources = Nothing  -- inherit True
      }
  }

task2 = Task
  { id = task2Id
  , identifier = "Worksheet-15.b"
  , content = Just "5(x - 2) = 15"
  , taskType = SubTask groupId $ TaskAttributesOverride
      { primary = Nothing
      , secondary = Just [problemSolvingLevel2]  -- override: add secondary
      , purpose = Nothing
      , displayInResources = Nothing
      }
  }
```

### 3. Recording evidence with tasks

```haskell
evidence = Evidence
  { id = evidenceId
  , userIds = [studentId]
  , tasks = [task1Id, task2Id]  -- completed Worksheet-15.a and 15.b
  , observations = ...  -- derived from task1 and task2 primary competences
  , ...
  }
```

### 4. Finding practice resources for a competence

```haskell
getResourcesForCompetence :: CompetenceLevelId -> Document -> [Task]
getResourcesForCompetence clId doc =
  filter (clId `elem` primary . getTaskAttributes) $
  filter (displayInResources . getTaskAttributes) $
  Ix.toList doc.tasks
  where
    getTaskAttributes :: Task -> TaskAttributes
    getTaskAttributes task = case task.taskType of
      SelfContained attrs -> attrs
      SubTask groupId override ->
        let group = Ix.getOne (Ix.getEQ groupId doc.taskGroups)
            defaults = group.defaultTaskAttributes
        in TaskAttributes
          { primary = fromMaybe defaults.primary override.primary
          , secondary = fromMaybe defaults.secondary override.secondary
          , purpose = fromMaybe defaults.purpose override.purpose
          , displayInResources = fromMaybe defaults.displayInResources override.displayInResources
          }
```

## Implementation Steps

### Phase 1: Tasks (Current Focus)

1. **Define data types** in `common/lib/Competences/Document/Task.hs`
   - TaskPurpose (Practice | Assessment)
   - TaskAttributes, TaskAttributesOverride
   - TaskGroup, Task, TaskType
   - Common accessor functions (getTaskAttributes, getTaskContent, etc.)
   - JSON instances

2. **Add to Document model**
   - `tasks :: IxSet TaskIxs Task`
   - `taskGroups :: IxSet TaskGroupIxs TaskGroup`

3. **Update Evidence**
   - Add `tasks :: [TaskId]` field
   - Add `oldTasks :: Maybe Text` for gradual migration
   - Keep existing `activityTasks` temporarily, migrate data to `oldTasks`

4. **Add Commands**
   - CreateTask, ModifyTask, DeleteTask
   - CreateTaskGroup, ModifyTaskGroup, DeleteTaskGroup

5. **Update UI**
   - Task/TaskGroup CRUD interface
   - Resource view (practice vs assessment tasks, separate sections)
   - Evidence task selection

### Phase 2: Assignments (After Tasks Complete)

1. **Define Assignment** in `common/lib/Competences/Document/Assignment.hs`
   - Assignment entity (to be refined)
   - AssignmentEvaluation (ephemeral)
   - JSON instances

2. **Add to Document model**
   - `assignments :: IxSet AssignmentIxs Assignment`

3. **Update projection** (temporarily hide from students)
   - `projectDocument` filters out assignments for students (Phase 2-3)

4. **Add Commands**
   - CreateAssignment, ModifyAssignment, DeleteAssignment

5. **Update UI**
   - Assignment creation/editing
   - Assignment evaluation workflow
   - Evidence creation from assignment evaluation

6. **Remove Template** (after Assignment is functional)
   - Delete Template/TemplateAspect/TemplateEvaluation code
   - Remove from Document model
   - Clean up CLAUDE.md references

### Phase 3: UI Polish

- Improve visual design
- Refine user workflows
- **First semi-productive release**

### Phase 4: Student Visibility

1. **Update projection**
   - Include assignments in student projected view
   - Filter by assignmentDate (hide future tests)
   - Include tasks in student view

2. **Student UI**
   - View assigned tasks
   - See task resources for competences

### Phase 5: Markup Language

1. **Define markup language** for task content
   - Rich text, math formulas, images, solutions

2. **Update Task.content** field
   - Parse and render markup

3. **Replace HTML exercise page**
   - System becomes primary distribution method for exercises

## Open Questions

None - all design questions have been resolved. See "Design Rationale" section below for key decisions.

## Design Rationale

This section documents key design decisions and their justifications:

**1. TaskGroup dual responsibility (content + attributes):**
- Maps real-world structure (textbooks, worksheets) naturally
- contentBefore/contentAfter enables "Solve the following: [tasks]" pattern
- Justified by actual teaching materials structure

**2. TaskAttributesOverride granularity:**
- School materials don't align perfectly with custom competence grids
- Some subtasks target higher levels or are practice-only
- Rare but necessary flexibility

**3. Evidence verbosity (listing individual tasks):**
- Enables task-by-task assessment in template workflow
- Tracks incomplete work (students skip tasks within groups)
- Reflects minimal homework practice (rarely full groups assigned)

**4. Non-unique identifiers:**
- Convention: textbook tasks by number (e.g., "1.2.3"), worksheets with prefix (e.g., "Ü20.1")
- User manages uniqueness, UUID provides fallback
- Pragmatic for real-world naming

**5. Secondary competences inclusion:**
- Real-world tasks test multiple competences simultaneously
- Need to track competences demonstrated in context
- Used sparingly but essential for completeness

**6. displayInResources separate from purpose:**
- Controls timing (e.g., exam questions hidden until after exam)
- Orthogonal to Practice/Assessment distinction
- Not redundant - serves different purpose

**7. TaskPurpose (Practice | Assessment):**
- Practice tasks help develop competence but aren't sufficient proof alone
- Assessment tasks clearly demonstrate competence achievement
- Practice tasks can be used in Evidence when multiple tasks or oral explanation provides sufficient proof
- Both shown in resources (separate sections) - students can practice with assessment tasks too

**8. Template → Assignment shift:**
- Tasks are rich enough to encode evaluation aspects
- Assignment more natural for student-facing system
- Concrete instances (date, students) better than reusable schemas
- Students should see what's assigned to them

## Future Enhancements

- **Markup language** for task.content (rich text, math formulas, images)
- **Task templates** for quickly creating similar tasks
- **Workload estimation** (assign time estimates to tasks)
- **Student progress tracking** (which tasks completed, when)
- **Adaptive suggestions** (recommend tasks based on student performance)

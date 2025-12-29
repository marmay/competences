# Tasks Implementation Plan

## Overview
This plan implements Phase 1 of the task system design (docs/TASKS-DESIGN.md). We'll work collaboratively, reviewing each step before proceeding.

## Status
**Phase 1: Partially Complete** (Steps 1-8 complete, Steps 9-12 partially complete)

### Completed:
- ✅ Steps 1-5: Core data types, IxSet indices, Document integration, Evidence migration
- ✅ Steps 6-7: Commands (TaskPatch, TaskGroupPatch, SubTaskPatch), AffectedUsers logic
- ✅ Step 8: Backend compilation verified
- ✅ Step 9 (Partial): SelfContainedTaskEditor with selector component
- ✅ Step 12 (Partial): Navigation integration (teacher-only route at `/tasks`)
- ✅ End-to-end testing completed

### Remaining:
- ⏳ Step 9: Add competences fields (primary/secondary) to task editor
- ⏳ Step 9: TaskGroupEditor component for managing task groups and subtasks
- ⏳ Step 10: Resource view integration (show tasks in competence grid)
- ⏳ Step 11: Evidence task selection (replace text input with task selector)
- ⏳ Step 13: Complete testing and documentation

## Phase 1: Tasks and TaskGroups

### Step 1: Define Core Data Types
**Location:** `common/lib/Competences/Document/Task.hs` (new file)

**What we'll create:**
1. `TaskPurpose` - Practice vs Assessment
2. `TaskAttributes` - Core attributes (primary/secondary competences, purpose, displayInResources)
3. `TaskAttributesOverride` - For subtask inheritance
4. `TaskGroup` - Organizational unit for related tasks
5. `Task` - Atomic work unit
6. `TaskType` - SelfContained vs SubTask
7. JSON instances for all types
8. Helper functions:
   - `getTaskAttributes :: Document -> Task -> TaskAttributes`
   - `getTaskContent :: Document -> Task -> Maybe Text`
   - `isResourceTask :: Document -> Task -> Bool`
   - `getTaskPrimaryCompetences :: Document -> Task -> [CompetenceLevelId]`
   - `getTaskAllCompetences :: Document -> Task -> [CompetenceLevelId]`
   - `getTasksInGroup :: TaskGroupId -> Document -> [Task]`

**Dependencies:** None - self-contained module

**Review checkpoint:** Verify types match design document exactly

---

### Step 2: Add IDs for Tasks and TaskGroups
**Location:** `common/lib/Competences/Document/Task.hs` (same file as Step 1)

**What we'll add:**
1. `TaskId` newtype with UUID
2. `TaskGroupId` newtype with UUID
3. IxSet index instances
4. JSON instances

**Dependencies:** Step 1 (will be in same file, so done together)

**Review checkpoint:** Verify IDs follow same pattern as existing IDs (UserId, EvidenceId, etc.)

---

### Step 3: Add IxSet Indices
**Location:** `common/lib/Competences/Document/Task.hs`

**What we'll add:**
1. `TaskIxs` type alias: `'[TaskId, Text, Maybe TaskGroupId]`
2. `Indexable` instance for Task
3. `TaskGroupIxs` type alias: `'[TaskGroupId, Text]`
4. `Indexable` instance for TaskGroup

**Dependencies:** Step 1, Step 2

**Review checkpoint:** Verify indices match design (TaskId, identifier, taskGroupId for Task; TaskGroupId, identifier for TaskGroup)

---

### Step 4: Integrate into Document Model
**Location:** `common/lib/Competences/Document.hs`

**What we'll modify:**
1. Import `Competences.Document.Task`
2. Add fields to Document:
   - `tasks :: IxSet TaskIxs Task`
   - `taskGroups :: IxSet TaskGroupIxs TaskGroup`
3. Update JSON instances to include new fields
4. Update `emptyDocument` to include empty IxSets

**Dependencies:** Step 1, Step 2, Step 3

**Review checkpoint:** Verify Document compiles and JSON round-trips correctly

---

### Step 5: Update Evidence Model (Gradual Migration)
**Location:** `common/lib/Competences/Document/Evidence.hs`

**What we'll modify:**
1. Add new field: `tasks :: [TaskId]`
2. Add migration field: `oldTasks :: Maybe Text`
3. Migrate existing `activityTasks` data:
   - In JSON instance: read old `activityTasks` into `oldTasks`
   - Keep empty `tasks` list for now
4. Update JSON instances to handle both old and new formats
5. Remove `activityTasks` field (or deprecate if risky)

**Dependencies:** Step 2 (TaskId)

**Review checkpoint:** Verify existing evidences load correctly with data in `oldTasks`

---

### Step 6: Define Command Types
**Location:** `common/lib/Competences/Command.hs`

**What we'll add:**
1. Add to `EntityCommand`:
   ```haskell
   | TaskCmd TaskId (ModifyCommand Task)
   | TaskGroupCmd TaskGroupId (ModifyCommand TaskGroup)
   ```
2. Update `handleCommand` to handle new command types
3. Implement Create/Delete/Modify for Task
4. Implement Create/Delete/Modify for TaskGroup
5. Validation logic:
   - Check TaskGroup exists when creating SubTask
   - Check no dependent Tasks when deleting TaskGroup
   - Handle identifier uniqueness (user-managed, warn if duplicate)

**Dependencies:** Step 1, Step 2, Step 4

**Review checkpoint:** Verify commands validate and apply correctly

---

### Step 7: Update AffectedUsers Logic
**Location:** `common/lib/Competences/Command.hs` (in `handleCommand`)

**What we'll add:**
For task commands, determine affected users:
- Currently: all task commands affect all teachers (students don't see tasks yet)
- Return `AffectedUsers [all teachers]`

**Dependencies:** Step 6

**Review checkpoint:** Verify teachers receive task command broadcasts

---

### Step 8: Backend - No Changes Needed
**Location:** `backend/lib/Competences/Backend/WebSocket.hs`

**What we'll verify:**
- Commands automatically saved via `updateDocument`
- Versioned envelopes handle new command types
- No backend code changes needed (commands are generic)

**Dependencies:** Step 6

**Review checkpoint:** Verify backend compiles and handles new commands

---

### Step 9: Frontend - Basic Task CRUD UI
**Location:** `frontend/lib/Competences/Frontend/Component/TaskManager.hs` (new file)

**What we'll create:**
1. TaskManager component with:
   - List view of all tasks and task groups
   - Create task (SelfContained or SubTask)
   - Create task group
   - Edit task/task group
   - Delete task/task group (with validation)
2. Task form with:
   - Identifier field
   - Content field (optional)
   - TaskType selection (SelfContained vs SubTask)
   - For SelfContained: TaskAttributes editor
   - For SubTask: TaskGroup selector + TaskAttributesOverride editor
3. TaskGroup form with:
   - Identifier field
   - Default attributes editor
   - Content before/after fields

**Dependencies:** Step 6 (commands)

**Review checkpoint:** Verify basic CRUD operations work end-to-end

---

### Step 10: Frontend - Resource View Integration
**Location:** `frontend/lib/Competences/Frontend/Component/CompetenceGridView.hs`

**What we'll modify:**
1. When clicking a competence cell, show:
   - Existing resources (external links)
   - **Practice tasks** (new section)
   - **Assessment tasks** (new section)
2. Filter tasks by:
   - `displayInResources = True`
   - `CompetenceLevelId ∈ task.primary`
3. Display task identifier and content (if available)

**Dependencies:** Step 9

**Review checkpoint:** Verify resource view shows tasks correctly

---

### Step 11: Frontend - Evidence Task Selection
**Location:** `frontend/lib/Competences/Frontend/Component/EvidenceEditor.hs`

**What we'll modify:**
1. Replace text input for tasks with task selector
2. Add `oldTasks` display (read-only, if present)
3. Task selector allows:
   - Search by identifier
   - Select multiple tasks
   - Preview task content
4. Display selected tasks with their identifiers

**Dependencies:** Step 9

**Review checkpoint:** Verify evidence can be created with selected tasks

---

### Step 12: Frontend - Navigation and Polish
**Location:** `frontend/lib/Competences/Frontend/App.hs`

**What we'll modify:**
1. Add route for TaskManager view
2. Add navigation link (teacher-only)
3. Polish UI:
   - Consistent styling with Tailwind
   - Loading states
   - Error messages
   - Validation feedback

**Dependencies:** Step 9, Step 10, Step 11

**Review checkpoint:** Full UI walkthrough

---

### Step 13: Testing and Documentation
**What we'll do:**
1. Test full workflow:
   - Create task group with multiple subtasks
   - Create standalone tasks
   - Use tasks in evidence
   - View tasks in resource view
2. Test edge cases:
   - Delete task group with subtasks (should fail)
   - Create subtask referencing non-existent group (should fail)
   - Load document with old evidence format (should migrate to oldTasks)
3. Update CLAUDE.md with new sections
4. Test database persistence (commands + snapshots)

**Dependencies:** All previous steps

**Review checkpoint:** Final verification before considering Phase 1 complete

---

## Decision Points

We'll pause at each step for review and discussion. Key decisions to make along the way:

1. **Step 1**: Confirm data types match your mental model
2. **Step 5**: Decide on migration strategy for existing evidence data
3. **Step 6**: Validate command handling and error messages
4. **Step 9**: Review UI design and user workflows
5. **Step 11**: Confirm task selection UX is intuitive

## Post-Phase 1

After Phase 1 is complete and tested:
- Document any issues encountered
- Plan Phase 2 (Assignments) based on lessons learned
- Consider whether to proceed with Phase 2 or release Phase 1 first

---

## Notes

- We'll compile after each step to catch errors early
- We'll test incrementally rather than waiting until the end
- We'll commit changes after each major step
- We'll update this plan if we discover better approaches

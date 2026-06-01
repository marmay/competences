{-# LANGUAGE OverloadedLabels #-}

-- | Match a freshly decoded 'ExchangeDoc' against the local 'Document'
-- to produce a structural preview the import modal can render.
-- Reuses the existing 'AssignmentImportPreview' / 'TaskImportPreview'
-- shapes so apply-side logic is shared with the legacy Markdown path.
module Competences.Exchange.Match
  ( -- * Preview ADT
    ExchangePreview (..)
  , GridPreview (..)
  , CompetencePreview (..)
  , LessonPreview (..)
  , TaskPreview (..)
  , AssignmentPreview (..)
    -- * Per-entity preview shapes
  , ImportAction (..)
  , AssignmentImportPreview (..)
  , TaskImportPreview (..)
  , ResourceImportPreview (..)
  , CompetenceMatch (..)
    -- * Entry points
  , matchExchangeDoc
  , previewHasChanges
  , previewHasBlockingConflicts
  , previewHasWarnings
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.Assignment (Assignment (..), AssignmentId, AssignmentName (..))
import Competences.Document.Competence (Competence (..), CompetenceLevelId, Level, LevelInfo (..))
import Competences.Document.CompetenceGrid (CompetenceGrid (..))
import Competences.Document.CompetenceLevelExample (CompetenceLevelExample (..))
import Competences.Document.Id (Id (..))
import Competences.Document.FileRef (FileRef (..), SHA256Hash (..))
import Competences.Document.Lesson (Lesson (..))
import Competences.Document.MesoPlan (MesoPlan (..))
import Competences.Document.Order (Order, orderMax, orderMin, orderPos)
import Competences.Document.Resource (Resource (..), ResourceContent (..), ResourceIdentifier (..))
import Competences.Document.Solution (Solution (..))
import Competences.Document.Task (Task (..), TaskIdentifier (..), defaultTask)
import Competences.Exchange.Types
  ( ExchangeAssignment (..)
  , ExchangeAttachment (..)
  , ExchangeCompetence (..)
  , ExchangeCompetenceGrid (..)
  , ExchangeCompetenceLevelExample (..)
  , ExchangeCompetenceRef (..)
  , ExchangeDoc (..)
  , ExchangeLesson (..)
  , ExchangeResource (..)
  , ExchangeResourceContent (..)
  , ExchangeSolution (..)
  , ExchangeTask (..)
  )
import Data.Map.Strict qualified as Map
import Competences.TaskContent.RichContent (fromTrustedInput, toRawText)
import GHC.Generics (Generic)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID.Types qualified as UUID
import Optics.Core ((&), (.~))

-- ============================================================================
-- Per-entity preview shapes (formerly in Competences.Import.Types)
-- ============================================================================

-- | An action to take during import.
data ImportAction a
  = Create !a
  | Update !a !a
  | NoChange !a
  | -- | Existing entity should be removed because the import doesn't
    -- carry it. Currently emitted only for competences during a grid
    -- update; the backend rejects deletes of in-use competences.
    Delete !a
  deriving (Eq, Show, Generic)

-- | Per-task preview: the matched action plus competence/solution
-- match results.
data TaskImportPreview = TaskImportPreview
  { taskAction :: !(ImportAction Task)
  , solutionActions :: ![ImportAction Solution]
  , competenceMatches :: ![CompetenceMatch]
  , secondaryCompetenceMatches :: ![CompetenceMatch]
  }
  deriving (Eq, Show, Generic)

-- | Result of matching a competence reference against the document.
data CompetenceMatch = CompetenceMatch
  { gridName :: !Text
  , description :: !Text
  , level :: !Level
  , matched :: !(Maybe CompetenceLevelId)
  }
  deriving (Eq, Show, Generic)

-- | Per-assignment preview: the matched action and the draft flag.
data AssignmentImportPreview = AssignmentImportPreview
  { assignmentAction :: !(ImportAction Assignment)
  , isDraft :: !Bool
  }
  deriving (Eq, Show, Generic)

-- | Per-resource preview: the matched action plus competence matches.
data ResourceImportPreview = ResourceImportPreview
  { resourceAction :: !(ImportAction Resource)
  , competenceMatches :: ![CompetenceMatch]
  }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- Top-level preview
-- ============================================================================

-- | Preview for a flat exchange document. One section per pool plus
-- two cross-cutting flag lists: 'conflicts' must be empty before
-- Apply is enabled; 'warnings' surface as a hold-to-confirm gate.
data ExchangePreview = ExchangePreview
  { gridPreviews :: ![GridPreview]
  , taskPreviews :: ![TaskPreview]
  , draftTaskPreviews :: ![TaskPreview]
  , assignmentPreviews :: ![AssignmentPreview]
  , draftAssignmentPreviews :: ![AssignmentPreview]
  , resourcePreviews :: ![ResourceImportPreview]
  , lessonPreviews :: ![LessonPreview]
  , conflicts :: ![Text]
  , warnings :: ![Text]
  }
  deriving (Eq, Show)

-- | Grid preview: the grid action plus per-competence previews. The
-- exchange grid is kept alongside so apply can re-walk the imported
-- competence list.
data GridPreview = GridPreview
  { exchangeGrid :: !ExchangeCompetenceGrid
  , gridAction :: !(ImportAction CompetenceGrid)
  , competencePreviews :: ![CompetencePreview]
  }
  deriving (Eq, Show)

-- | Per-competence preview: the competence action plus its
-- level-example actions. Examples follow replace-all-per-level
-- semantics, so 'exampleActions' contains only 'Create', 'Delete', and
-- 'NoChange' — never 'Update'.
data CompetencePreview = CompetencePreview
  { competenceAction :: !(ImportAction Competence)
  , exampleActions :: ![ImportAction CompetenceLevelExample]
  }
  deriving (Eq, Show)

-- | A task preview alongside the original payload — apply needs the
-- payload to look up identifiers when wiring assignments and lessons.
data TaskPreview = TaskPreview
  { exchangeTask :: !ExchangeTask
  , isDraft :: !Bool
  , preview :: !TaskImportPreview
  }
  deriving (Eq, Show)

-- | Assignment preview alongside the original payload. Apply uses
-- 'exchangeAssignment.taskRefs' to look up the assignment's tasks in
-- the freshly-applied task map.
data AssignmentPreview = AssignmentPreview
  { exchangeAssignment :: !ExchangeAssignment
  , preview :: !AssignmentImportPreview
  }
  deriving (Eq, Show)

-- | Lesson preview row + the original payload (so apply can resolve
-- phase item refs against the matched id maps).
data LessonPreview = LessonPreview
  { lesson :: !ExchangeLesson
  , lessonAction :: !(ImportAction Lesson)
  , competenceMatches :: ![CompetenceMatch]
  }
  deriving (Eq, Show)

-- | Match every section of the document independently, then layer
-- conflict / warning analysis across the union.
matchExchangeDoc :: Document -> ExchangeDoc -> ExchangePreview
matchExchangeDoc doc xdoc =
  let gps = map (matchExchangeGrid doc) xdoc.competenceGrids
      tps = map (\t -> TaskPreview t False (matchExchangeTask doc False t)) xdoc.tasks
      dtps = map (\t -> TaskPreview t True (matchExchangeTask doc True t)) xdoc.draftTasks
      aps = map (matchAssignmentPreview doc False) xdoc.assignments
      daps = map (matchAssignmentPreview doc True) xdoc.draftAssignments
      rps = map (matchExchangeResource doc) xdoc.resources
      lps = map (matchExchangeLesson doc) xdoc.lessons
      conflictList = lessonAssignmentConflicts doc xdoc
      warningList = assignmentRelinkWarnings doc xdoc
   in ExchangePreview
        { gridPreviews = gps
        , taskPreviews = tps
        , draftTaskPreviews = dtps
        , assignmentPreviews = aps
        , draftAssignmentPreviews = daps
        , resourcePreviews = rps
        , lessonPreviews = lps
        , conflicts = conflictList
        , warnings = warningList
        }

previewHasChanges :: ExchangePreview -> Bool
previewHasChanges p =
  any gridHasChanges p.gridPreviews
    || any (taskHasChanges . (.preview)) p.taskPreviews
    || any (taskHasChanges . (.preview)) p.draftTaskPreviews
    || any (assignmentHasChanges . (.preview)) p.assignmentPreviews
    || any (assignmentHasChanges . (.preview)) p.draftAssignmentPreviews
    || any (isChange . (.resourceAction)) p.resourcePreviews
    || any (isChange . (.lessonAction)) p.lessonPreviews

gridHasChanges :: GridPreview -> Bool
gridHasChanges gp =
  isChange gp.gridAction || any competencePreviewHasChanges gp.competencePreviews

competencePreviewHasChanges :: CompetencePreview -> Bool
competencePreviewHasChanges cp =
  isChange cp.competenceAction || any isChange cp.exampleActions

previewHasBlockingConflicts :: ExchangePreview -> Bool
previewHasBlockingConflicts p = not (null p.conflicts)

previewHasWarnings :: ExchangePreview -> Bool
previewHasWarnings p = not (null p.warnings)

-- ============================================================================
-- Cross-section conflict / warning analysis
-- ============================================================================

-- | An assignment carried by the import is already linked to a lesson
-- *not* in the import → we'd silently steal it. Hard conflict.
lessonAssignmentConflicts :: Document -> ExchangeDoc -> [Text]
lessonAssignmentConflicts doc xdoc =
  let importedLessonTitles =
        Set.fromList (map (normalize . (.title)) xdoc.lessons)
      check xa = case findAssignmentByName doc False xa.name of
        Nothing -> Nothing
        Just existing -> case lessonOwning doc existing.id of
          Nothing -> Nothing
          Just owner
            | normalize owner.title `Set.member` importedLessonTitles -> Nothing
            | otherwise ->
                Just $
                  "Assignment '"
                    <> xa.name
                    <> "' is already linked to lesson '"
                    <> owner.title
                    <> "' which is not part of this import."
   in mapMaybe check xdoc.assignments

-- | An assignment update where re-linking might confuse the user (for
-- example: matching by name when there's also a same-named assignment
-- under a different MesoPlan). Soft warning — surfaces as a
-- hold-to-confirm above Apply.
assignmentRelinkWarnings :: Document -> ExchangeDoc -> [Text]
assignmentRelinkWarnings doc xdoc =
  let dupNames =
        [ xa.name
        | xa <- xdoc.assignments
        , length
            (filter
              (\a -> let AssignmentName n = a.name in normalize n == normalize xa.name)
              (Ix.toList doc.assignments))
            > 1
        ]
   in map
        (\n -> "Multiple existing assignments named '" <> n <> "'; the first match will be updated.")
        dupNames

lessonOwning :: Document -> AssignmentId -> Maybe Lesson
lessonOwning doc aid =
  find (\l -> aid `elem` l.assignments) (Ix.toList doc.lessons)

-- ============================================================================
-- Per-entity matchers
-- ============================================================================

matchAssignmentPreview :: Document -> Bool -> ExchangeAssignment -> AssignmentPreview
matchAssignmentPreview doc isDraft a =
  AssignmentPreview
    { exchangeAssignment = a
    , preview = matchExchangeAssignment doc isDraft a
    }

-- ============================================================================
-- Grid matching (with replaces + per-competence Delete on absence)
-- ============================================================================

matchExchangeGrid :: Document -> ExchangeCompetenceGrid -> GridPreview
matchExchangeGrid doc xg =
  let existing = findGridWithReplaces doc xg
      gridAction = case existing of
        Nothing -> Create (makeNewGrid xg)
        Just g ->
          let updated = updateGrid g xg
           in if gridEquals g updated
                then NoChange g
                else Update g updated
      competencePreviews = matchCompetences doc existing xg.competences
   in GridPreview
        { exchangeGrid = xg
        , gridAction = gridAction
        , competencePreviews = competencePreviews
        }

findGridWithReplaces :: Document -> ExchangeCompetenceGrid -> Maybe CompetenceGrid
findGridWithReplaces doc xg =
  let byReplaces = case xg.replaces of
        Just oldTitle -> findGridByTitle doc oldTitle
        Nothing -> Nothing
      byTitle = findGridByTitle doc xg.title
   in byReplaces <|> byTitle

makeNewGrid :: ExchangeCompetenceGrid -> CompetenceGrid
makeNewGrid xg =
  CompetenceGrid
    { id = Id UUID.nil
    , order = orderMax
    , title = xg.title
    , description = xg.description
    }

updateGrid :: CompetenceGrid -> ExchangeCompetenceGrid -> CompetenceGrid
updateGrid existing xg =
  existing & #title .~ xg.title & #description .~ xg.description

gridEquals :: CompetenceGrid -> CompetenceGrid -> Bool
gridEquals a b = a.title == b.title && a.description == b.description

-- | For each imported competence, look up an existing one (by
-- 'replaces' first, then by 'description') within the matched grid.
-- Existing competences not present in the import emit a Delete; the
-- backend rejects deletes of in-use competences. Each preview also
-- carries the competence's level-example actions.
matchCompetences
  :: Document
  -> Maybe CompetenceGrid
  -> [ExchangeCompetence]
  -> [CompetencePreview]
matchCompetences doc maybeGrid xcs =
  let existingComps = case maybeGrid of
        Just g -> Ix.toList (doc.competences Ix.@= g.id)
        Nothing -> []
      lookupExisting xc =
        let byReplaces = case xc.replaces of
              Just old -> find (descMatches old) existingComps
              Nothing -> Nothing
            byDesc = find (descMatches xc.description) existingComps
         in byReplaces <|> byDesc
      importedPreviews =
        map
          ( \xc ->
              let existing = lookupExisting xc
                  action = case existing of
                    Nothing -> Create (makeNewCompetence xc)
                    Just c ->
                      let updated = updateCompetence c xc
                       in if competenceEquals c updated
                            then NoChange c
                            else Update c updated
               in CompetencePreview
                    { competenceAction = action
                    , exampleActions = matchExamples doc existing xc.examples
                    }
          )
          xcs
      matchedIds = mapMaybe (fmap (.id) . lookupExisting) xcs
      toDelete = filter (\c -> c.id `notElem` matchedIds) existingComps
   in importedPreviews
        ++ map (\c -> CompetencePreview (Delete c) []) toDelete

-- | Replace-all-per-level matching for a competence's examples. For
-- each level *present* in the imported map: if the existing examples
-- already equal the imported set, keep them ('NoChange'); otherwise
-- delete the existing ones and create the imported ones. Levels absent
-- from the import are left untouched (no actions emitted).
matchExamples
  :: Document
  -> Maybe Competence
  -> Map.Map Level [ExchangeCompetenceLevelExample]
  -> [ImportAction CompetenceLevelExample]
matchExamples doc maybeComp xExamples =
  concatMap perLevel (Map.toList xExamples)
  where
    compId = maybe (Id UUID.nil) (.id) maybeComp
    perLevel (level, xes) =
      let existing = case maybeComp of
            Just c -> Ix.toAscList (Proxy @Order) (doc.competenceLevelExamples Ix.@= (c.id, level))
            Nothing -> []
       in if examplesUnchanged existing xes
            then map NoChange existing
            else
              map Delete existing
                ++ zipWith (makeNewExample compId level) [0 ..] xes

-- | True when the existing examples already match the imported list
-- exactly (same content and attachments, same order).
examplesUnchanged :: [CompetenceLevelExample] -> [ExchangeCompetenceLevelExample] -> Bool
examplesUnchanged existing xes =
  length existing == length xes
    && and (zipWith sameExample existing xes)
  where
    sameExample e xe =
      toRawText e.content == xe.content
        && e.attachments == map attachmentFromExchange xe.attachments

makeNewExample
  :: Id Competence -> Level -> Int -> ExchangeCompetenceLevelExample -> ImportAction CompetenceLevelExample
makeNewExample compId level i xe =
  Create
    CompetenceLevelExample
      { id = Id UUID.nil
      , competenceId = compId
      , level = level
      , order = orderPos i
      , content = fromTrustedInput xe.content
      , attachments = map attachmentFromExchange xe.attachments
      }

descMatches :: Text -> Competence -> Bool
descMatches d c = normalize c.description == normalize d

makeNewCompetence :: ExchangeCompetence -> Competence
makeNewCompetence xc =
  Competence
    { id = Id UUID.nil
    , competenceGridId = Id UUID.nil
    , order = orderMax
    , description = xc.description
    , levels = fmap (\desc -> LevelInfo{description = desc, locked = False}) xc.levels
    }

updateCompetence :: Competence -> ExchangeCompetence -> Competence
updateCompetence existing xc =
  existing
    & #description .~ xc.description
    & #levels .~ mergeLevels existing.levels xc.levels

mergeLevels :: Map.Map Level LevelInfo -> Map.Map Level Text -> Map.Map Level LevelInfo
mergeLevels existing updates =
  let newInfos = fmap (\desc -> LevelInfo{description = desc, locked = False}) updates
   in Map.unionWith (\_ new -> new) existing newInfos

competenceEquals :: Competence -> Competence -> Bool
competenceEquals a b =
  a.description == b.description
    && fmap (.description) a.levels == fmap (.description) b.levels

findGridByTitle :: Document -> Text -> Maybe CompetenceGrid
findGridByTitle doc title =
  find (\g -> normalize g.title == normalize title) (Ix.toList doc.competenceGrids)

(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> y = y
x <|> _ = x

-- ============================================================================
-- Assignment / task / resource / lesson matchers (with replaces)
-- ============================================================================

matchExchangeAssignment :: Document -> Bool -> ExchangeAssignment -> AssignmentImportPreview
matchExchangeAssignment doc isDraft a =
  let existing = findAssignmentWithReplaces doc isDraft a
      assignmentAction = case existing of
        Nothing -> Create (makeNewAssignment a)
        Just e ->
          let updated = updateAssignment e a
           in if assignmentEquals e updated
                then NoChange e
                else Update e updated
   in AssignmentImportPreview
        { assignmentAction = assignmentAction
        , isDraft = isDraft
        }

matchExchangeTask :: Document -> Bool -> ExchangeTask -> TaskImportPreview
matchExchangeTask doc isDraft t =
  let existing = findTaskWithReplaces doc isDraft t
      taskAction = case existing of
        Nothing -> Create (makeNewTask t)
        Just e ->
          let updated = updateTask e t
           in if taskEquals e updated
                then NoChange e
                else Update e updated
      taskId = case existing of
        Just e -> e.id
        Nothing -> Id UUID.nil
      existingSolutions = Ix.toList (doc.solutions Ix.@= taskId)
      solutionActions = matchSolutions existingSolutions t.solutions
      primaryMatches = map (matchCompetence doc) t.primary
      secondaryMatches = map (matchCompetence doc) t.secondary
   in TaskImportPreview
        { taskAction = taskAction
        , solutionActions = solutionActions
        , competenceMatches = primaryMatches
        , secondaryCompetenceMatches = secondaryMatches
        }

matchExchangeResource :: Document -> ExchangeResource -> ResourceImportPreview
matchExchangeResource doc r =
  let existing = findResourceWithReplaces doc r
      action = case existing of
        Nothing -> Create (makeNewResource r)
        Just e ->
          let updated = updateResource e r
           in if resourceEquals e updated
                then NoChange e
                else Update e updated
   in ResourceImportPreview
        { resourceAction = action
        , competenceMatches = map (matchCompetence doc) r.competenceLevels
        }

matchExchangeLesson :: Document -> ExchangeLesson -> LessonPreview
matchExchangeLesson doc l =
  let existing = findLessonWithReplaces doc l
      lessonAction = case existing of
        Nothing -> Create (makeNewLesson doc l)
        Just e ->
          let updated = updateLesson e l
           in if lessonEquals e updated
                then NoChange e
                else Update e updated
   in LessonPreview
        { lesson = l
        , lessonAction = lessonAction
        , competenceMatches = map (matchCompetence doc) l.competences
        }

-- ============================================================================
-- Lookups
-- ============================================================================

findAssignmentByName :: Document -> Bool -> Text -> Maybe Assignment
findAssignmentByName doc isDraft name =
  let pool = if isDraft then doc.draftAssignments else doc.assignments
   in find (\a -> let AssignmentName n = a.name in normalize n == normalize name) (Ix.toList pool)

findAssignmentWithReplaces :: Document -> Bool -> ExchangeAssignment -> Maybe Assignment
findAssignmentWithReplaces doc isDraft a =
  let byReplaces = case a.replaces of
        Just old -> findAssignmentByName doc isDraft old
        Nothing -> Nothing
      byName = findAssignmentByName doc isDraft a.name
   in byReplaces <|> byName

findTaskByIdentifier :: Document -> Bool -> TaskIdentifier -> Maybe Task
findTaskByIdentifier doc isDraft ident =
  let pool = if isDraft then doc.draftTasks else doc.tasks
   in Ix.getOne (pool Ix.@= ident)

findTaskWithReplaces :: Document -> Bool -> ExchangeTask -> Maybe Task
findTaskWithReplaces doc isDraft t =
  let byReplaces = case t.replaces of
        Just old -> findTaskByIdentifier doc isDraft (TaskIdentifier old)
        Nothing -> Nothing
      byIdent = findTaskByIdentifier doc isDraft (TaskIdentifier t.identifier)
   in byReplaces <|> byIdent

findResourceByIdentifier :: Document -> Text -> Maybe Resource
findResourceByIdentifier doc ident =
  find
    (\r -> let ResourceIdentifier i = r.identifier in normalize i == normalize ident)
    (Ix.toList doc.resources)

findResourceWithReplaces :: Document -> ExchangeResource -> Maybe Resource
findResourceWithReplaces doc r =
  let byReplaces = case r.replaces of
        Just old -> findResourceByIdentifier doc old
        Nothing -> Nothing
      byIdent = findResourceByIdentifier doc r.identifier
   in byReplaces <|> byIdent

findLessonByTitle :: Document -> Text -> Maybe Lesson
findLessonByTitle doc title =
  find (\l -> normalize l.title == normalize title) (Ix.toList doc.lessons)

findLessonWithReplaces :: Document -> ExchangeLesson -> Maybe Lesson
findLessonWithReplaces doc l =
  let byReplaces = case l.replaces of
        Just old -> findLessonByTitle doc old
        Nothing -> Nothing
      byTitle = findLessonByTitle doc l.title
   in byReplaces <|> byTitle

-- ============================================================================
-- Builders
-- ============================================================================

makeNewAssignment :: ExchangeAssignment -> Assignment
makeNewAssignment a =
  Assignment
    { id = Id UUID.nil
    , name = AssignmentName a.name
    , description = fromTrustedInput a.description
    , assignmentDate = a.assignmentDate
    , activityType = a.activityType
    , studentIds = Set.empty
    , tasks = []
    , groupSubmissionAllowed = a.groupSubmissionAllowed
    }

updateAssignment :: Assignment -> ExchangeAssignment -> Assignment
updateAssignment existing a =
  Assignment
    { id = existing.id
    , name = AssignmentName a.name
    , description = fromTrustedInput a.description
    , assignmentDate = a.assignmentDate
    , activityType = a.activityType
    , studentIds = existing.studentIds
    , tasks = existing.tasks
    , groupSubmissionAllowed = a.groupSubmissionAllowed
    }

assignmentEquals :: Assignment -> Assignment -> Bool
assignmentEquals x y =
  x.name == y.name
    && x.description == y.description
    && x.assignmentDate == y.assignmentDate
    && x.activityType == y.activityType
    && x.groupSubmissionAllowed == y.groupSubmissionAllowed

makeNewTask :: ExchangeTask -> Task
makeNewTask t =
  defaultTask (Id UUID.nil)
    & #identifier .~ TaskIdentifier t.identifier
    & #title .~ t.title
    & #content .~ fmap fromTrustedInput t.content
    & #purpose .~ t.purpose
    & #attachments .~ map attachmentFromExchange t.attachments

updateTask :: Task -> ExchangeTask -> Task
updateTask existing t =
  existing
    & #identifier .~ TaskIdentifier t.identifier
    & #title .~ t.title
    & #content .~ fmap fromTrustedInput t.content
    & #purpose .~ t.purpose
    & #attachments .~ map attachmentFromExchange t.attachments

taskEquals :: Task -> Task -> Bool
taskEquals x y =
  x.identifier == y.identifier
    && x.title == y.title
    && x.content == y.content
    && x.purpose == y.purpose
    && x.attachments == y.attachments

-- | Reconstruct a 'FileRef' from the attachment metadata. Same-server
-- imports rely on the shared CAS being able to serve the blob from
-- 'sha256'; cross-server imports surface the file as missing until
-- embedded-content support lands.
attachmentFromExchange :: ExchangeAttachment -> FileRef
attachmentFromExchange a =
  FileRef
    { hash = SHA256Hash a.sha256
    , fileName = a.fileName
    , mimeType = a.mimeType
    , fileSize = a.bytes
    }

-- | Replace-by-type for solutions: any existing solution whose type
-- matches an incoming type is queued for update; types not in the
-- import are preserved untouched.
matchSolutions :: [Solution] -> [ExchangeSolution] -> [ImportAction Solution]
matchSolutions existing incoming =
  let action s =
        case find (\e -> e.solutionType == s.solutionType) existing of
          Just e -> Update e (replaceContent e s)
          Nothing -> Create (makeNewSolution s)
   in map action incoming

replaceContent :: Solution -> ExchangeSolution -> Solution
replaceContent old s =
  old
    & #solutionType .~ s.solutionType
    & #content .~ fromTrustedInput s.content

makeNewSolution :: ExchangeSolution -> Solution
makeNewSolution s =
  Solution
    { id = Id UUID.nil
    , taskId = Id UUID.nil
    , userId = Id UUID.nil
    , solutionType = s.solutionType
    , content = fromTrustedInput s.content
    , files = []
    }

makeNewResource :: ExchangeResource -> Resource
makeNewResource r =
  Resource
    { id = Id UUID.nil
    , identifier = ResourceIdentifier r.identifier
    , competenceLevels = []
    , content = fromExchangeContent r.content
    , attachments = map attachmentFromExchange r.attachments
    }

updateResource :: Resource -> ExchangeResource -> Resource
updateResource existing r =
  Resource
    { id = existing.id
    , identifier = ResourceIdentifier r.identifier
    , competenceLevels = existing.competenceLevels
    , content = fromExchangeContent r.content
    , attachments = map attachmentFromExchange r.attachments
    }

fromExchangeContent :: ExchangeResourceContent -> ResourceContent
fromExchangeContent = \case
  ExInlineContent t -> InlineContent (fromTrustedInput t)
  ExWebLink u d -> WebLink u d
  ExVideoLink u d -> VideoLink u d
  ExFileContent a -> FileContent (attachmentFromExchange a)

resourceEquals :: Resource -> Resource -> Bool
resourceEquals a b =
  a.identifier == b.identifier
    && a.content == b.content
    && a.attachments == b.attachments

makeNewLesson :: Document -> ExchangeLesson -> Lesson
makeNewLesson doc l =
  Lesson
    { id = Id UUID.nil
    , mesoPlanId = case Ix.toList doc.mesoPlans of
        (mp : _) -> mp.id
        [] -> Id UUID.nil
    , order = orderMin
    , title = l.title
    , description = fromTrustedInput l.description
    , competenceLevels = []
    , date = l.date
    , assignments = []
    , phases = []
    , supplementalItems = []
    , notesTitleOverride = l.notesTitleOverride
    , privateNoteRef = Nothing
    }

updateLesson :: Lesson -> ExchangeLesson -> Lesson
updateLesson existing l =
  existing
    & #title .~ l.title
    & #description .~ fromTrustedInput l.description
    & #date .~ l.date
    & #notesTitleOverride .~ l.notesTitleOverride

lessonEquals :: Lesson -> Lesson -> Bool
lessonEquals a b =
  a.title == b.title
    && a.description == b.description
    && a.date == b.date
    && a.notesTitleOverride == b.notesTitleOverride

-- ============================================================================
-- Helpers
-- ============================================================================

assignmentHasChanges :: AssignmentImportPreview -> Bool
assignmentHasChanges p = isChange p.assignmentAction

taskHasChanges :: TaskImportPreview -> Bool
taskHasChanges tp =
  isChange tp.taskAction || any isChange tp.solutionActions

isChange :: ImportAction a -> Bool
isChange (Create _) = True
isChange (Update _ _) = True
isChange (NoChange _) = False
isChange (Delete _) = True

matchCompetence :: Document -> ExchangeCompetenceRef -> CompetenceMatch
matchCompetence doc ref =
  let mGrid = find (\g -> normalize g.title == normalize ref.grid) (Ix.toList doc.competenceGrids)
      mComp = case mGrid of
        Nothing -> Nothing
        Just grid ->
          find
            (\c -> c.competenceGridId == grid.id && normalize c.description == normalize ref.description)
            (Ix.toList doc.competences)
   in CompetenceMatch
        { gridName = ref.grid
        , description = ref.description
        , level = ref.level :: Level
        , matched = (\c -> (c.id, ref.level)) <$> mComp :: Maybe CompetenceLevelId
        }

normalize :: Text -> Text
normalize = T.toLower . T.strip

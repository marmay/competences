-- | Server-side translator between the structured 'ExchangeDoc' IR
-- and YAML text. Frontend builds/consumes 'ExchangeDoc' via 'Binary';
-- this module converts to the clipboard-side YAML payload.
module Competences.Backend.Exchange
  ( documentToExchange
  , exchangeToYaml
  , handleRequestExport
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Assignment (..)
  , Competence (..)
  , CompetenceGrid (..)
  , Document (..)
  , Solution (..)
  , Task (..)
  )
import Competences.Document.Assignment (AssignmentName (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.FileRef (FileRef (..), SHA256Hash (..))
import Competences.Document.Task (TaskId, TaskIdentifier (..))
import Competences.Exchange.Types
  ( ExchangeAssignment (..)
  , ExchangeAttachment (..)
  , ExchangeCompetenceRef (..)
  , ExchangeDoc (..)
  , ExchangeSolution (..)
  , ExchangeTask (..)
  )
import Competences.Protocol (ExportTarget (..), ServerMessage (..))
import Competences.TaskContent.RichContent (toRawText)
import Data.Binary qualified as Bin
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Yaml qualified as Yaml
import Network.WebSockets qualified as WS

-- | Build an 'ExchangeDoc' from the document for a given export
-- target. Returns a 'Left' with a short reason when the target is
-- unresolvable (missing entity, wrong collection, etc.).
documentToExchange :: Document -> ExportTarget -> Either Text ExchangeDoc
documentToExchange doc = \case
  ExportAssignment aid isDraft ->
    let pool = if isDraft then doc.draftAssignments else doc.assignments
     in case Ix.getOne (pool Ix.@= aid) of
          Nothing -> Left "assignment not found"
          Just a -> Right (ExchangeAssignmentDoc (assignmentToExchange doc isDraft a))

-- | YAML-encode an 'ExchangeDoc' via its 'ToJSON' instance.
exchangeToYaml :: ExchangeDoc -> Text
exchangeToYaml = decodeUtf8 . Yaml.encode

-- | Handle a 'RequestExport' by responding with 'ExportText' or
-- 'ExportFailed' on the given WebSocket connection.
handleRequestExport :: WS.Connection -> Document -> ExportTarget -> IO ()
handleRequestExport conn doc target =
  case documentToExchange doc target of
    Left reason -> WS.sendBinaryData conn (Bin.encode (ExportFailed reason))
    Right xdoc ->
      WS.sendBinaryData conn (Bin.encode (ExportText (exchangeToYaml xdoc)))

-- ============================================================================
-- Pure builders
-- ============================================================================

assignmentToExchange :: Document -> Bool -> Assignment -> ExchangeAssignment
assignmentToExchange doc isDraft a =
  let AssignmentName name = a.name
      tasks' = mapMaybe (lookupTask doc) a.tasks
   in ExchangeAssignment
        { name = name
        , description = toRawText a.description
        , assignmentDate = a.assignmentDate
        , activityType = a.activityType
        , isDraft = isDraft
        , groupSubmissionAllowed = a.groupSubmissionAllowed
        , tasks = map (taskToExchange doc) tasks'
        }

lookupTask :: Document -> TaskId -> Maybe Task
lookupTask doc tid =
  Ix.getOne (doc.tasks Ix.@= tid)

taskToExchange :: Document -> Task -> ExchangeTask
taskToExchange doc t =
  let TaskIdentifier ident = t.identifier
      solutions = Ix.toList (doc.solutions Ix.@= t.id)
   in ExchangeTask
        { identifier = ident
        , title = t.title
        , content = fmap toRawText t.content
        , purpose = t.purpose
        , primary = mapMaybe (competenceRef doc) t.primary
        , secondary = mapMaybe (competenceRef doc) t.secondary
        , solutions = map solutionToExchange solutions
        , attachments = map attachmentToExchange t.attachments
        }

solutionToExchange :: Solution -> ExchangeSolution
solutionToExchange s =
  ExchangeSolution
    { solutionType = s.solutionType
    , content = toRawText s.content
    }

attachmentToExchange :: FileRef -> ExchangeAttachment
attachmentToExchange fref =
  ExchangeAttachment
    { fileName = fref.fileName
    , mimeType = fref.mimeType
    , sha256 = fref.hash.unSHA256Hash
    , bytes = fref.fileSize
    }

competenceRef :: Document -> CompetenceLevelId -> Maybe ExchangeCompetenceRef
competenceRef doc (cid, level) = do
  comp <- Ix.getOne (doc.competences Ix.@= cid)
  grid <- Ix.getOne (doc.competenceGrids Ix.@= comp.competenceGridId)
  pure
    ExchangeCompetenceRef
      { grid = grid.title
      , description = comp.description
      , level = level
      }

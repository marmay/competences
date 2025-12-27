module Competences.Frontend.Component.EvidenceEditor
  ( evidenceEditorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Common.IxSet qualified as IxSet
import Competences.Document
  ( Document (..)
  , Evidence (..)
  , Lock (..)
  , User (..)
  )
-- ActivityTasks removed - using oldTasks field now
import Competences.Document.User (isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Selector.Common (selectorTransformedLens)
import Competences.Frontend.Component.Selector.EvidenceSelector (evidenceSelectorComponent)
import Competences.Frontend.Component.Selector.ObservationSelector qualified as TE
import Competences.Frontend.Component.Selector.UserSelector (multiUserEditorField)
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (?~), (^.))
import Optics.Core qualified as O
import qualified Competences.Frontend.View.Tailwind as T

data Model = Model
  { evidence :: !(Maybe Evidence)
  }
  deriving (Eq, Generic, Show)

data Action

evidenceEditorComponent :: SyncDocumentRef -> M.Component p Model Action
evidenceEditorComponent r =
  M.component model update view
  where
    model = Model Nothing
    update _ = pure ()

    view m =
      V.sideMenu
        (V.componentA "evidence-editor-selection" [T.tailwind [T.HFull]] (evidenceSelectorComponent r #evidence))
        (V.componentA evidenceEditorId [T.tailwind [T.HFull]] (TE.editorComponent evidenceEditor r))
      where
        evidenceEditorId = "evidence-editor-editor-" <> maybe "empty" (M.ms . show . (.id)) m.evidence
        evidenceEditable =
          TE.editable
            ( \d -> do
                e <- m.evidence
                fmap
                  (\c -> (c, (d ^. #locks) Map.!? EvidenceLock c.id))
                  (Ix.getOne $ d.evidences Ix.@= e.id)
            )
            & (#modify ?~ (\e modify -> OnEvidences (Modify e.id modify)))
            & (#delete ?~ (\e -> OnEvidences (Delete e.id)))
        evidenceEditor =
          TE.editor
            ( TE.editorFormView'
                (C.translate' C.LblEditEvidence)
                id
            )
            evidenceEditable
            `TE.addNamedField` ( C.translate' C.LblEvidenceDate
                               , TE.dayEditorField #date
                               )
            `TE.addNamedField` ( C.translate' C.LblActivityType
                               , TE.enumEditorField
                                   (C.translate' . C.LblActivityTypeDescription)
                                   #activityType
                               )
            `TE.addNamedField` ( C.translate' C.LblStudents
                               , multiUserEditorField
                                   r
                                   (evidenceEditorId <> "-users")
                                   isStudent
                                   (selectorTransformedLens (.id) Set.fromList #userIds)
                               )
            `TE.addNamedField` ( "Legacy Tasks (read-only)"  -- TODO: Better label from translation
                               , TE.textEditorField (#oldTasks O.% oldTasksIso)
                               )
            `TE.addNamedField` ( C.translate' C.LblActivityObservations
                               , TE.observationEditorField
                                   r
                                   (evidenceEditorId <> "-observations")
                                   (.id)
                                   (selectorTransformedLens id IxSet.fromList #observations)
                               )
        oldTasksIso :: O.Iso' (Maybe Text) M.MisoString
        oldTasksIso = O.iso
          (\mt -> maybe "" M.ms mt)
          (\ms -> let t = M.fromMisoString ms :: Text in if t == "" then Nothing else Just t)

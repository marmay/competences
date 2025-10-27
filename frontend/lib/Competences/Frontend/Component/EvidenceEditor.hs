module Competences.Frontend.Component.EvidenceEditor
  ( evidenceEditorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , Evidence (..)
  , Lock (..)
  , User (..)
  )
import Competences.Document.Evidence (ActivityTasks (..))
import Competences.Document.User (isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Selector.Common (selectorTransformedLens)
import Competences.Frontend.Component.Selector.EvidenceSelector (evidenceSelectorComponent)
import Competences.Frontend.Component.Selector.UserSelector (multiUserEditorField)
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (.~), (?~), (^.))
import Optics.Core qualified as O

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
      V.viewFlow
        (V.hFlow & #expandDirection .~ V.Expand V.Start)
        [ V.component "evidence-editor-selection" (evidenceSelectorComponent r #evidence)
        , V.component evidenceEditorId (TE.editorComponent evidenceEditor r)
        ]
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
            `TE.addNamedField` ( C.translate' C.LblActivityTasks
                               , TE.textEditorField (#activityTasks O.% activityTasksIso)
                               )
        activityTasksIso :: O.Iso' ActivityTasks M.MisoString
        activityTasksIso = O.iso (\(ActivityTasks t) -> M.ms t) (ActivityTasks . M.fromMisoString)

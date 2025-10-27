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
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Selector.EvidenceSelector (evidenceSelectorComponent)
import Competences.Frontend.SyncDocument
  ( SyncDocumentEnv (..)
  , SyncDocumentRef
  , modifySyncDocument
  , nextId
  , syncDocumentEnv
  )
import Competences.Frontend.View qualified as V
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((&), (.~), (?~), (^.), (%))
import Competences.Frontend.Component.Selector.UserSelector (multiUserSelectorComponent)
import Competences.Document.User (isStudent)
import qualified Optics.Core as O
import qualified Data.Set as Set
import Competences.Frontend.Component.Selector.Common (selectorTransformedLens)

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
        , V.component
            ("evidence-editor-editor-" <> maybe "empty" (M.ms . show . (.id)) m.evidence)
            (TE.editorComponent evidenceEditor r)
        ]
      where
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
            `TE.addNamedField` ( "Users"
                               , TE.hostEditorField (selectorTransformedLens (Set.fromList . fmap (.id)) #userIds)
                                                    (const $ M.div_ [] [])
                                                    (\e -> multiUserSelectorComponent r isStudent ((`Set.member` e.userIds) . (.id)))
                               )

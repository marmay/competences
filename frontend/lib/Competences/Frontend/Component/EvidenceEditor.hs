module Competences.Frontend.Component.EvidenceEditor
  ( -- evidenceEditorComponent
  )
where

import Competences.Command (Command (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , Evidence (..)
  , EvidenceId
  , User
  )
import Competences.Document.Evidence (mkEvidence)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncDocumentRef, modifySyncDocument)
import Control.Monad (when)
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((&), (.~))
import UnliftIO (liftIO)

-- data Model = Model
--   { evidence :: !(Maybe Evidence)
--   , nextObservationText :: !Text
--   }
--   deriving (Eq, Generic, Show)
-- 
-- data Action
--   = UpdateDocument DocumentChange
--   | AddObservation
--   deriving (Eq, Show)
-- 
-- evidenceEditorComponent :: SyncDocumentRef -> User -> EvidenceId -> M.Component p Model Action
-- evidenceEditorComponent r u eId = M.component model update view
--   where
--     model = Model Nothing ""
-- 
--     update (UpdateDocument (DocumentChange d _)) =
--       M.modify $
--         \m -> case Ix.getOne (d.evidences Ix.@= eId) of
--                 Just e -> m & (#evidence .~ Right e)
--                 Nothing -> m
-- 
--     view (Model (Left e) t) = M.text_ [C.translate' C.LblInitializing]
--     view (Model (Right e) t) = M.div_ [] []

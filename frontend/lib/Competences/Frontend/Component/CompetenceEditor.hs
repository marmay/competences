module Competences.Frontend.Component.CompetenceEditor
  ( Model (..)
  , Action (..)
  , competenceEditorComponent
  )
where

import Competences.Command (Command (..))
import Competences.Document.Competence (Competence (..), Level (..), levels)
import Competences.Frontend.Common (Label (..), translate')
import Competences.Frontend.SyncDocument (SyncDocumentRef, modifySyncDocument)
import Competences.Frontend.View (applyLabelButton, cancelLabelButton, modalDialog)
import Competences.Frontend.View.Form qualified as V
import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core (Lens', at, non, (%), (.~), (^.))
import qualified Miso.Html.Property as M

newtype Model = Model
  { competence :: Competence
  }
  deriving (Eq, Show, Generic)

data Action
  = ChangeDescription !M.MisoString
  | ChangeLevelDescription !Level !M.MisoString
  | CompleteEditing
  | CancelEditing
  deriving (Eq, Show, Generic)

competenceEditorComponent
  :: forall closeMessage parent
   . (ToJSON closeMessage)
  => closeMessage -> SyncDocumentRef -> Competence -> M.Component parent Model Action
competenceEditorComponent closeMessage syncDocumentRef competence =
  M.component model update view
  where
    model = Model competence

    update (ChangeDescription s) = M.modify (descriptionLens .~ M.fromMisoString s)
    update (ChangeLevelDescription l s) = M.modify (levelDescriptionLens l .~ M.fromMisoString s)
    update CompleteEditing = do
      c <- M.gets (^. #competence)
      M.io_ $ modifySyncDocument syncDocumentRef $ AddCompetence c
      M.mailParent closeMessage
    update CancelEditing = M.mailParent closeMessage

    view m =
      let descriptionField =
            V.formField_
              (translate' LblCompetenceDescription)
              $ textField ChangeDescription (Right $ M.ms $ m ^. descriptionLens)
          levelTextField level =
            V.formField_ (translate' $ LblCompetenceLevelDescription level) $
              textField
                (ChangeLevelDescription level)
                (withPlaceholder (translate' $ LblCompetenceLevelPlaceholder level) (M.ms $ m ^. levelDescriptionLens level))
       in modalDialog
            []
            [ V.form_
                (translate' LblEditCompetence)
                ([descriptionField] <> map levelTextField levels)
                [ applyLabelButton [M.onClick CompleteEditing]
                , cancelLabelButton [M.onClick CancelEditing]
                ]
            ]

withPlaceholder :: M.MisoString -> M.MisoString -> Either M.MisoString M.MisoString
withPlaceholder placeholder "" = Left placeholder
withPlaceholder _ s = Right s

textField :: (M.MisoString -> Action) -> Either M.MisoString M.MisoString -> M.View m Action
textField a (Left s) = textField' a (M.placeholder_ (M.ms s))
textField a (Right s) = textField' a (M.value_ (M.ms s))

textField' :: (M.MisoString -> Action) -> M.Attribute Action -> M.View m Action
textField' a attr = V.textarea_ [M.onInput a, attr]

descriptionLens :: Lens' Model Text
descriptionLens = #competence % #description

levelDescriptionLens :: Level -> Lens' Model Text
levelDescriptionLens l = #competence % #levelDescriptions % at l % non ""

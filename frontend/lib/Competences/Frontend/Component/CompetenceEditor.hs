module Competences.Frontend.Component.CompetenceEditor
  ( Model (..)
  , Action (..)
  , update
  , view
  )
where

import Competences.Command (Command (..))
import Competences.Document.Competence (Competence (..), Level (..))
import Competences.Frontend.Common (Label (..), translate')
import Competences.Frontend.SyncDocument (SyncDocumentRef, modifySyncDocument)
import Competences.Frontend.View (applyLabelButton, cancelLabelButton, modalDialog)
import Competences.Frontend.View.Form qualified as V
import Data.Either.Extra (maybeToEither)
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String qualified as M
import Optics.Core (Lens', at, (%), (.~), (^.))

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

update :: SyncDocumentRef -> Action -> M.Effect p Model Action
update _ (ChangeDescription s) = do
  M.modify (descriptionLens .~ M.fromMisoString s)
update _ (ChangeLevelDescription l s) = do
  M.modify (levelDescriptionLens l .~ toMaybe s)
update r CompleteEditing = do
  c <- M.gets (^. #competence)
  M.io_ $ modifySyncDocument r $ AddCompetence c
update _ _ = pure ()

view :: Model -> M.View m Action
view m =
  let descriptionField =
        V.formField_
          (translate' LblCompetenceDescription)
          $ textField ChangeDescription (Right $ M.ms $ m ^. descriptionLens)
      levelTextField level =
        V.formField_ (translate' $ LblCompetenceLevelDescription level) $
          textField
            (ChangeLevelDescription level)
            (maybeToEither (translate' $ LblCompetenceLevelPlaceholder level) (m ^. levelDescriptionLens level))
   in modalDialog
        []
        [ V.form_
            (translate' LblEditCompetence)
            [ descriptionField
            , levelTextField BasicLevel
            , levelTextField IntermediateLevel
            , levelTextField AdvancedLevel
            ]
            [ applyLabelButton [M.onClick CompleteEditing]
            , cancelLabelButton [M.onClick CancelEditing]
            ]
        ]

textField :: (M.MisoString -> Action) -> Either M.MisoString M.MisoString -> M.View m Action
textField a (Left s) = textField' a (M.placeholder_ s)
textField a (Right s) = textField' a (M.value_ s)

textField' :: (M.MisoString -> Action) -> M.Attribute Action -> M.View m Action
textField' a attr = V.textarea_ [M.onInput a, attr]

toMaybe :: (M.FromMisoString a) => M.MisoString -> Maybe a
toMaybe "" = Nothing
toMaybe s = Just $ M.fromMisoString s

descriptionLens :: Lens' Model Text
descriptionLens = #competence % #description

levelDescriptionLens :: Level -> Lens' Model (Maybe Text)
levelDescriptionLens l = #competence % #levelDescriptions % at l

module Competences.Frontend.Component.CompetenceEditor
  ( Model (..)
  , Action (..)
  , update
  , view
  )
where

import Competences.Document.Competence (Competence (..), Level (..))
import Competences.Frontend.Common
  ( Icon (..)
  , Label (..)
  , TranslationData
  , iconLabelButton
  , translate
  )
import Competences.Frontend.SyncDocument (SyncDocumentRef, modifySyncDocument)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String qualified as M
import Optics.Core (Lens', at, non, (%), (.~), (^.))
import Competences.Command (Command(..))

data Model = Model
  { competence :: !Competence
  , translationData :: !TranslationData
  }
  deriving (Eq, Show, Generic)

data Action
  = ChangeDescription !M.MisoString
  | ChangeBasicLevelDescription !M.MisoString
  | ChangeIntermediateLevelDescription !M.MisoString
  | ChangeAdvancedLevelDescription !M.MisoString
  | CompleteEditing
  | CancelEditing
  deriving (Eq, Show, Generic)

update :: SyncDocumentRef -> Action -> M.Effect Model Action
update _ (ChangeDescription s) =
  M.modify (descriptionLens .~ M.fromMisoString s)
update _ (ChangeBasicLevelDescription s) =
  M.modify (basicLevelDescriptionLens .~ toMaybe s)
update _ (ChangeIntermediateLevelDescription s) =
  M.modify (intermediateLevelDescriptionLens .~ toMaybe s)
update _ (ChangeAdvancedLevelDescription s) =
  M.modify (advancedLevelDescriptionLens .~ toMaybe s)
update r CompleteEditing = do
  c <- M.gets (^. #competence)
  M.io_ $ modifySyncDocument r $ AddCompetence c
update _ _ = pure ()

view :: Model -> M.View Action
view m =
  let title = M.text $ translate m LblEditCompetence
      descriptionField = textField LblCompetenceDescription m descriptionLens ChangeDescription
      basicLevelDescriptionField =
        textField
          LblCompetenceBasicLevelDescription
          m
          (toMaybe' basicLevelDescriptionLens)
          ChangeBasicLevelDescription
      intermediateLevelDescriptionField =
        textField
          LblCompetenceIntermediateLevelDescription
          m
          (toMaybe' intermediateLevelDescriptionLens)
          ChangeIntermediateLevelDescription
      advancedLevelDescriptionField =
        textField
          LblCompetenceAdvancedLevelDescription
          m
          (toMaybe' advancedLevelDescriptionLens)
          ChangeAdvancedLevelDescription
      applyButton = iconLabelButton [M.onClick CompleteEditing] IcnApply (translate m LblApplyChange)
      cancelButton = iconLabelButton [M.onClick CancelEditing] IcnCancel (translate m LblCancelChange)
      buttons =
        M.div_
          []
          [ applyButton
          , cancelButton
          ]
   in M.div_
        []
        [ title
        , descriptionField
        , basicLevelDescriptionField
        , intermediateLevelDescriptionField
        , advancedLevelDescriptionField
        , buttons
        ]

textField :: Label -> Model -> Lens' Model M.Text -> (M.MisoString -> Action) -> M.View Action
textField lbl m l a =
  M.label_
    []
    [ M.text $ M.toMisoString $ translate m lbl
    , M.input_ [M.onInput a, M.value_ (M.toMisoString $ m ^. l)]
    ]

toMaybe :: (M.FromMisoString a) => M.MisoString -> Maybe a
toMaybe "" = Nothing
toMaybe s = Just $ M.fromMisoString s

toMaybe' :: Lens' Model (Maybe M.Text) -> Lens' Model M.Text
toMaybe' l = l % non ""

descriptionLens :: Lens' Model M.Text
descriptionLens = #competence % #description

levelDescriptionLens :: Level -> Lens' Model (Maybe M.Text)
levelDescriptionLens l = #competence % #levelDescriptions % at l

basicLevelDescriptionLens
  , intermediateLevelDescriptionLens
  , advancedLevelDescriptionLens
    :: Lens' Model (Maybe M.Text)
basicLevelDescriptionLens = levelDescriptionLens BasicLevel
intermediateLevelDescriptionLens = levelDescriptionLens IntermediateLevel
advancedLevelDescriptionLens = levelDescriptionLens AdvancedLevel

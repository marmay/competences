module Competences.Frontend.Component.CompetenceEditor
  ( Model (..)
  , Action (..)
  , update
  , view
  )
where

import Competences.Command (Command (..))
import Competences.Document.Competence (Competence (..), Level (..))
import Competences.Frontend.Common
  ( Icon (..)
  , Label (..)
  , TranslationData
  , iconLabelButton
  , translate
  )
import Competences.Frontend.Common.Style qualified as C
import Competences.Frontend.SyncDocument (SyncDocumentRef, modifySyncDocument)
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String qualified as M
import Optics.Core (Lens', at, (%), (.~), (^.))
import System.IO.Unsafe (unsafePerformIO)

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

update :: SyncDocumentRef -> Action -> M.Effect p Model Action
update _ (ChangeDescription s) = do
  let x = unsafePerformIO $ putStrLn ("ChangeDescription" <> show s)
  M.modify (x `seq` descriptionLens .~ M.fromMisoString s)
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

view :: Model -> M.View m Action
view m =
  let title = M.div_ [C.styledClass C.ClsTitle] [M.text $ translate m LblEditCompetence]
      descriptionField =
        textField (translate m LblCompetenceDescription) ChangeDescription (Just m.competence.description)
      basicLevelDescriptionField =
        textField
          (translate m LblCompetenceBasicLevelDescription)
          ChangeBasicLevelDescription
          (m.competence.levelDescriptions ^. at BasicLevel)
      intermediateLevelDescriptionField =
        textField
          (translate m LblCompetenceIntermediateLevelDescription)
          ChangeIntermediateLevelDescription
          (m.competence.levelDescriptions ^. at IntermediateLevel)
      advancedLevelDescriptionField =
        textField
          (translate m LblCompetenceAdvancedLevelDescription)
          ChangeAdvancedLevelDescription
          (m.competence.levelDescriptions ^. at AdvancedLevel)
      applyButton = iconLabelButton [M.onClick CompleteEditing] IcnApply (translate m LblApplyChange)
      cancelButton = iconLabelButton [M.onClick CancelEditing] IcnCancel (translate m LblCancelChange)
      buttons =
        M.div_
          [ C.styledClass C.ClsButtonRow ]
          [ applyButton
          , cancelButton
          ]
   in M.div_
        [C.styledClass C.ClsModal60]
        [ title
        , descriptionField
        , basicLevelDescriptionField
        , intermediateLevelDescriptionField
        , advancedLevelDescriptionField
        , buttons
        ]

textField :: M.MisoString -> (M.MisoString -> Action) -> Maybe Text -> M.View m Action
textField lbl a v =
  M.div_
    [C.styledClass C.ClsFormRow]
    [ M.label_
        [C.styledClass C.ClsFormItem]
        [ M.span_ [] [M.text lbl]
        , M.textarea_ [M.onInput a, M.value_ (maybe "" M.ms v)] []
        ]
    ]

toMaybe :: (M.FromMisoString a) => M.MisoString -> Maybe a
toMaybe "" = Nothing
toMaybe s = Just $ M.fromMisoString s

descriptionLens :: Lens' Model Text
descriptionLens = #competence % #description

levelDescriptionLens :: Level -> Lens' Model (Maybe Text)
levelDescriptionLens l = #competence % #levelDescriptions % at l

basicLevelDescriptionLens
  , intermediateLevelDescriptionLens
  , advancedLevelDescriptionLens
    :: Lens' Model (Maybe Text)
basicLevelDescriptionLens = levelDescriptionLens BasicLevel
intermediateLevelDescriptionLens = levelDescriptionLens IntermediateLevel
advancedLevelDescriptionLens = levelDescriptionLens AdvancedLevel

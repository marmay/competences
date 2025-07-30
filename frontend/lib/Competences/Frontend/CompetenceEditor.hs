module Competences.Frontend.CompetenceEditor
  ( OutMail (..)
  , CompetenceEditorComponent
  , competenceEditor
  )
where

import Competences.Document.Competence (Competence (..), Level (..))
import Competences.Frontend.Common.Translate (Label (..), TranslationData, translate)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String qualified as M
import Optics.Core (Lens', at, non, (%), (.~), (^.))
import Competences.Frontend.Common.Button (iconLabelButton)
import Competences.Frontend.Common.Icon (Icon(..))

data OutMail
  = EditingCanceled
  | EditingDone !Competence
  deriving (Eq, Show, Generic)

instance FromJSON OutMail

instance ToJSON OutMail

competenceEditor :: ToJSON a => Competence -> TranslationData -> (OutMail -> a) -> M.Component Model Action
competenceEditor competence translationData translateMessage =
  M.component (Model competence translationData) update view
  where
    update (ChangeDescription s) =
      M.modify (descriptionLens .~ M.fromMisoString s)
    update (ChangeBasicLevelDescription s) =
      M.modify (basicLevelDescriptionLens .~ toMaybe s)
    update (ChangeIntermediateLevelDescription s) =
      M.modify (intermediateLevelDescriptionLens .~ toMaybe s)
    update (ChangeAdvancedLevelDescription s) =
      M.modify (advancedLevelDescriptionLens .~ toMaybe s)
    update CompleteEditing = do
      c <- M.gets (.competence)
      M.issue $ NotifyParent $ EditingDone c
    update CancelEditing = M.issue $ NotifyParent $ EditingCanceled
    update (NotifyParent m) = M.getParentComponentId (Notify m) (Log "No parent component found")
    update (Notify m cid) = do
      M.io_ $ M.consoleLog $ "We want to send " <> M.toMisoString (show m) <> " to " <> M.toMisoString (show cid)
      M.mail 1 (translateMessage m)
    update (Log msg) = M.io_ $ M.consoleLog msg

    toMaybe "" = Nothing
    toMaybe s = Just $ M.fromMisoString s

    toMaybe' l = l % non ""

    descriptionLens = #competence % #description
    levelDescriptionLens l = #competence % #levelDescriptions % at l
    basicLevelDescriptionLens = levelDescriptionLens BasicLevel
    intermediateLevelDescriptionLens = levelDescriptionLens IntermediateLevel
    advancedLevelDescriptionLens = levelDescriptionLens AdvancedLevel

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
          buttons = M.div_ [] [ applyButton
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

type CompetenceEditorComponent = M.Component Model Action

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
  | NotifyParent !OutMail
  | Notify !OutMail !M.ComponentId
  | Log !M.MisoString

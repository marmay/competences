module Competences.Frontend.Component.Selector.EvidenceSelector
  ( evidenceSelectorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Evidence, EvidenceIxs, User (..), UserId)
import Competences.Document.Evidence
  ( ActivityTasks (..)
  , Evidence (..)
  , mkEvidence
  )
import Competences.Document.User (isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.Common (selectorLens)
import Competences.Frontend.Component.Selector.EnumSelector qualified as ES
import Competences.Frontend.Component.Selector.UserSelector
  ( SingleUserSelectorStyle (SingleUserSelectorStyleButtons)
  , UserSelectorConfig (..)
  , defaultUserSelectorConfig
  , singleUserSelectorComponent
  )
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentEnv (..)
  , SyncDocumentRef
  , modifySyncDocument
  , nextId
  , subscribeDocument
  , syncDocumentEnv
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind qualified as T
import Data.Foldable (toList)
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core (Lens', toLensVL, (&), (.~), (?~), (^.))

data DateRange
  = Today
  | ThisWeek
  | AllTime
  deriving (Eq, Show)

data Model = Model
  { filteredUsers :: !(Maybe User)
  , filteredDateRange :: !DateRange
  , allEvidences :: Ix.IxSet EvidenceIxs Evidence
  , userNames :: Map.Map UserId M.MisoString
  , selectedEvidence :: !(Maybe Evidence)
  , newEvidence :: !(Maybe Evidence)
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectEvidence !Evidence
  | CreateNewEvidence
  | UpdateDocument !DocumentChange
  deriving (Eq, Show)

evidenceSelectorComponent
  :: SyncDocumentRef -> Lens' p (Maybe Evidence) -> M.Component p Model Action
evidenceSelectorComponent r parentLens =
  (M.component model update view)
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedEvidence]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = Model Nothing ThisWeek Ix.empty Map.empty Nothing Nothing
    update (SelectEvidence e) =
      M.modify $ \m -> case Ix.getOne (m.allEvidences Ix.@= e.id) of
        Just e' -> m & (#selectedEvidence ?~ e') & (#newEvidence .~ Nothing)
        Nothing -> m & (#newEvidence ?~ e)
    update CreateNewEvidence = M.withSink $ \s -> do
      evidenceId <- nextId r
      let today = syncDocumentEnv r ^. #currentDay
      let evidence = mkEvidence evidenceId today
      modifySyncDocument r (OnEvidences $ Create evidence)
      modifySyncDocument r (OnEvidences $ Modify evidenceId Lock)
      s (SelectEvidence evidence)
    update (UpdateDocument (DocumentChange d _)) = M.modify $ updateModel d

    updateModel :: Document -> Model -> Model
    updateModel d m =
      let allEvidences' = d.evidences
          userNames' = Map.fromList $ map (\u -> (u.id, M.ms u.name)) (Ix.toList d.users)
          validateEvidence e = do
            e' <- e
            Ix.getOne $ allEvidences' Ix.@= e'.id
          (selectedEvidence', newEvidence') = case (validateEvidence m.selectedEvidence, validateEvidence m.newEvidence) of
            (_, Just e) -> (Just e, Nothing)
            (s, n) -> (s, n)
       in m
            { allEvidences = allEvidences'
            , userNames = userNames'
            , selectedEvidence = selectedEvidence'
            , newEvidence = newEvidence'
            }

    view m =
      V.viewFlow
        (V.vFlow & (#gap .~ V.SmallSpace) & (#expandDirection .~ V.Expand V.Start) & (#extraAttrs .~ [T.tailwind [T.HFull]]))
        [ V.title_ (C.translate' C.LblSelectEvidences)
        , V.component
            "evidence-selector-users"
            ( singleUserSelectorComponent
                r
                defaultUserSelectorConfig {isPossibleUser = isStudent}
                SingleUserSelectorStyleButtons
                (selectorLens #filteredUsers)
            )
        , V.component
            "evidence-selector-date-range"
            ( ES.enumSelectorComponent'
                AllTime
                [Today, ThisWeek, AllTime]
                ES.ButtonsCompact
                translateDateRange
                #filteredDateRange
            )
        , viewEvidences m
        , V.viewButton (V.labelButton' C.LblAddEvidence CreateNewEvidence)
        ]
    viewEvidences m =
      let filteredEvidences = Ix.toList m.allEvidences
       in V.viewFlow
            (V.vFlow & (#gap .~ V.SmallSpace) & (#extraAttrs .~ [T.tailwind [T.OverflowYScroll, T.MinH0]]))
            (map viewEvidence filteredEvidences)
      where
        viewEvidence e =
          M.a_
            [C.onClick' (SelectEvidence e)]
            [ V.viewFlow
                (V.vFlow & (#expandOrthogonal .~ V.Expand V.Start))
                ( V.viewFlow
                    (V.hFlow & (#expandDirection .~ V.Expand V.Start))
                    [ viewDate e.date
                    , V.flowSpring
                    , V.text_ (viewActivityType e.activityType)
                    ]
                    : [ viewContext [] (commaSeparated $ sort $ mapMaybe (`Map.lookup` m.userNames) (toList e.userIds))
                      , viewContext [] (viewActivityTasks e.activityTasks)
                      ]
                )
            ]
        viewDate d = V.text_ (C.formatDay d)
        viewActivityType = C.translate' . C.LblActivityTypeDescription
        viewActivityTasks (ActivityTasks t) = M.ms t
        viewContext extraAttrs ms = M.span_ ([] <> extraAttrs) [V.text_ ms]
        commaSeparated  :: [M.MisoString] -> M.MisoString
        commaSeparated (x:x':xs) = x <> ", " <> commaSeparated (x':xs)
        commaSeparated [x] = x
        commaSeparated [] = ""

    translateDateRange Today = C.translate' C.LblToday
    translateDateRange ThisWeek = C.translate' C.LblThisWeek
    translateDateRange AllTime = C.translate' C.LblAllTime

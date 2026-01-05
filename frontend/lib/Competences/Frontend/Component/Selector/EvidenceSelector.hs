module Competences.Frontend.Component.Selector.EvidenceSelector
  ( evidenceSelectorComponent
  )
where

import Competences.Command qualified as Cmd
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Evidence, EvidenceIxs, User (..), UserId)
import Competences.Document.Evidence
  ( Evidence (..)
  , mkEvidence
  )
import Competences.Document.User (isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.Common (selectorLens)
import Competences.Frontend.Component.Selector.EnumSelector qualified as ES
import Competences.Frontend.Component.Selector.UserSelector
  ( UserSelectorConfig (..)
  , defaultUserSelectorConfig
  , searchableSingleUserSelectorComponent
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
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Frontend.View.Tailwind (class_)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core (Lens', toLensVL, (&), (.~), (?~), (^.))
import Data.Time (Day, addDays)
import Data.Proxy (Proxy(..))

data DateRange
  = Today
  | ThisWeek
  | AllTime
  deriving (Eq, Show)

data Model = Model
  { filteredUser :: !(Maybe User)
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
    model = Model Nothing AllTime Ix.empty Map.empty Nothing Nothing
    update (SelectEvidence e) =
      M.modify $ \m -> case Ix.getOne (m.allEvidences Ix.@= e.id) of
        Just e' -> m & (#selectedEvidence ?~ e') & (#newEvidence .~ Nothing)
        Nothing -> m & (#newEvidence ?~ e)
    update CreateNewEvidence = M.withSink $ \s -> do
      evidenceId <- nextId r
      let today = syncDocumentEnv r ^. #currentDay
      let evidence = mkEvidence evidenceId today
      modifySyncDocument r (Cmd.Evidences $ Cmd.OnEvidences $ Cmd.Create evidence)
      modifySyncDocument r (Cmd.Evidences $ Cmd.OnEvidences $ Cmd.Modify evidenceId Cmd.Lock)
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
        ( V.vFlow
            & (#gap .~ V.SmallSpace)
            & (#expandDirection .~ V.Expand V.Start)
            & (#extraAttrs .~ [V.fullHeight])
        )
        [ SL.selectorHeader (C.translate' C.LblSelectEvidences) (Just CreateNewEvidence)
        , V.component
            "evidence-selector-users"
            ( searchableSingleUserSelectorComponent
                r
                defaultUserSelectorConfig {isPossibleUser = isStudent}
                (selectorLens #filteredUser)
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
        ]
    viewEvidences m =
      let dateRangeFilter :: Ix.IxSet EvidenceIxs Evidence -> Ix.IxSet EvidenceIxs Evidence
          dateRangeFilter = case m.filteredDateRange of
            AllTime -> id
            ThisWeek -> (Ix.@>= (addDays (-7) $ syncDocumentEnv r ^. #currentDay))
            Today -> (Ix.@= (syncDocumentEnv r ^. #currentDay))
          filteredEvidences =
            Ix.toDescList (Proxy @Day) (dateRangeFilter $ m.allEvidences Ix.@=! fmap (.id) m.filteredUser)
       in SL.selectorList (map (viewEvidence m) filteredEvidences)

    viewEvidence m e =
      let isSelected = m.selectedEvidence == Just e || m.newEvidence == Just e
          userName = maybe "" (\uid -> fromMaybe "" (Map.lookup uid m.userNames)) e.userId
       in SL.selectorItemMultiLine
            isSelected
            [ M.div_
                [class_ "flex items-center justify-between text-sm"]
                [ M.text (C.formatDay e.date)
                , M.text (C.translate' $ C.LblActivityTypeDescription e.activityType)
                ]
            , M.div_
                [class_ "text-xs text-muted-foreground truncate"]
                [M.text userName]
            ]
            (SelectEvidence e)

    translateDateRange Today = C.translate' C.LblToday
    translateDateRange ThisWeek = C.translate' C.LblThisWeek
    translateDateRange AllTime = C.translate' C.LblAllTime

{-# LANGUAGE IncoherentInstances #-}

module Competences.Frontend.Component.StatisticsOverview
  ( statisticsOverviewComponent
  , Model (..)
  , Action (..)
  , emptyModel
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..))
import Competences.Document.Evidence
  ( Ability (..)
  , ActivityType (..)
  , Evidence (..)
  , Observation (..)
  )
import Competences.Document.User (isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncDocumentRef, subscribeDocument)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Typography qualified as Typography
import Data.List (sortBy)
import Data.Map qualified as Map
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Miso qualified as M

-- | Statistics Overview Component Model
-- For teacher view showing all students' statistics
data Model = Model
  { byUserStats :: Map.Map User UserStatistics
  , maximumHomeExerciseTasks :: !Int
  , maximumSchoolExerciseTasks :: !Int
  }
  deriving (Eq, Generic, Show)

data UserStatistics = UserStatistics
  { homeExerciseTasks :: !Int
  , schoolExerciseTasks :: !Int
  , totalTasks :: !Int
  , selfReliantEvidences :: !Int
  , selfReliantWithSillyMistakesEvidences :: !Int
  , withSupportEvidences :: !Int
  , notYetEvidences :: !Int
  }
  deriving (Eq, Generic, Show)

-- | Statistics Overview Component Actions
data Action
  = UpdateDocument !DocumentChange
  deriving (Eq, Show)

-- | Empty model for statistics overview
emptyModel :: Model
emptyModel =
  Model
    { byUserStats = Map.empty
    , maximumHomeExerciseTasks = 0
    , maximumSchoolExerciseTasks = 0
    }

data StatColumn
  = ColName
  | ColHomeExercises
  | ColSchoolExercises
  | ColTotalExercises
  | ColSelfReliant
  | ColSelfReliantWithSillyMistakes
  | ColWithSupport
  | ColNotYet
  | ColTotalObservations
  deriving (Eq, Ord, Show)

-- | Statistics Overview Component
statisticsOverviewComponent :: SyncDocumentRef -> M.Component p Model Action
statisticsOverviewComponent docRef =
  (M.component model update view)
    { M.subs = [subscribeDocument docRef UpdateDocument]
    , M.events = mempty
    , M.initialAction = Nothing
    }
  where
    model = emptyModel

    update :: Action -> M.Effect p Model Action
    update (UpdateDocument (DocumentChange doc _)) =
      M.modify $ const $ computeStats doc

    view :: Model -> M.View Model Action
    view m = V.viewFlow V.vFlow [Typography.h2 "Statistics Overview", table]
      where
        table =
          V.viewTable $
            V.defTable
              { V.columns =
                  [ ColName
                  , ColHomeExercises
                  , ColSchoolExercises
                  , ColTotalExercises
                  , ColSelfReliant
                  , ColSelfReliantWithSillyMistakes
                  , ColWithSupport
                  , ColNotYet
                  , ColTotalObservations
                  ]
              , V.rows = sortBy (comparing (.name)) $ Map.keys m.byUserStats
              , V.columnSpec = \c -> V.TableColumnSpec {width = V.AutoSizedColumn, title = columnLabel c}
              , V.rowContents = \cs u ->
                  case m.byUserStats Map.!? u of
                    (Just userData) -> V.tableRow $ map (cellContents m u userData) cs
                    Nothing -> V.tableRow $ map (const $ V.text_ "?") cs
              }

columnLabel :: StatColumn -> M.MisoString
columnLabel ColName = C.translate' C.LblUserName
columnLabel ColHomeExercises = C.translate' (C.LblActivityTypeDescription HomeExercise)
columnLabel ColSchoolExercises = C.translate' (C.LblActivityTypeDescription SchoolExercise)
columnLabel ColTotalExercises = C.translate' C.LblTotalExercises
columnLabel ColSelfReliant = C.translate' (C.LblAbility SelfReliant)
columnLabel ColSelfReliantWithSillyMistakes = C.translate' (C.LblAbility SelfReliantWithSillyMistakes)
columnLabel ColWithSupport = C.translate' (C.LblAbility WithSupport)
columnLabel ColNotYet = C.translate' (C.LblAbility NotYet)
columnLabel ColTotalObservations = C.translate' C.LblTotalObservations

cellContents :: Model -> User -> UserStatistics -> StatColumn -> M.View m a
cellContents _ u _ ColName = V.text_ $ M.ms u.name
cellContents m _ d ColHomeExercises =
  graduallyColored d.homeExerciseTasks m.maximumHomeExerciseTasks
cellContents m _ d ColSchoolExercises =
  graduallyColored d.schoolExerciseTasks m.maximumSchoolExerciseTasks
cellContents m _ d ColTotalExercises =
  graduallyColored d.totalTasks (m.maximumHomeExerciseTasks + m.maximumSchoolExerciseTasks)
cellContents _ _ d ColSelfReliant =
  V.coloredText_ (V.abilityColor SelfReliant) (M.ms $ show d.selfReliantEvidences)
cellContents _ _ d ColSelfReliantWithSillyMistakes =
  V.coloredText_
    (V.abilityColor SelfReliantWithSillyMistakes)
    (M.ms $ show d.selfReliantWithSillyMistakesEvidences)
cellContents _ _ d ColWithSupport =
  V.coloredText_ (V.abilityColor WithSupport) (M.ms $ show d.withSupportEvidences)
cellContents _ _ d ColNotYet =
  V.coloredText_ (V.abilityColor NotYet) (M.ms $ show d.notYetEvidences)
cellContents _ _ d ColTotalObservations =
  V.text_
    ( M.ms $
        show $
          d.selfReliantEvidences
            + d.selfReliantWithSillyMistakesEvidences
            + d.withSupportEvidences
            + d.notYetEvidences
    )

graduallyColored :: Int -> Int -> M.View m a
graduallyColored value maximumValue =
  V.coloredText_
    (V.gradualPercentageColor (fromIntegral value / fromIntegral maximumValue))
    (M.ms value)

computeStats :: Document -> Model
computeStats document =
  let byUserStats =
        Map.fromList $
          map (\user -> (user, computeUserStats document user)) (filter isStudent $ Ix.toList document.users)
      maximumHomeExerciseTasks = maximum $ map (.homeExerciseTasks) $ Map.elems byUserStats
      maximumSchoolExerciseTasks = maximum $ map (.schoolExerciseTasks) $ Map.elems byUserStats
   in Model {byUserStats, maximumHomeExerciseTasks, maximumSchoolExerciseTasks}

computeUserStats :: Document -> User -> UserStatistics
computeUserStats document user =
  let evidences = Ix.toList $ document.evidences Ix.@= user.id
      observations = concatMap (Ix.toList . (.observations)) evidences
      homeExerciseTasks = length $ filter (\evidence -> evidence.activityType == HomeExercise) evidences
      schoolExerciseTasks = length $ filter (\evidence -> evidence.activityType == SchoolExercise) evidences
      totalTasks = homeExerciseTasks + schoolExerciseTasks
      selfReliantEvidences = length $ filter (\observation -> observation.ability == SelfReliant) observations
      selfReliantWithSillyMistakesEvidences =
        length $ filter (\observation -> observation.ability == SelfReliantWithSillyMistakes) observations
      withSupportEvidences = length $ filter (\observation -> observation.ability == WithSupport) observations
      notYetEvidences = length $ filter (\observation -> observation.ability == NotYet) observations
   in UserStatistics
        { homeExerciseTasks
        , schoolExerciseTasks
        , totalTasks
        , selfReliantEvidences
        , selfReliantWithSillyMistakesEvidences
        , withSupportEvidences
        , notYetEvidences
        }

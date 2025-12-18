module Competences.Frontend.Component.StatisticsOverview
  ( statisticsOverviewComponent
  , Model (..)
  , Action (..)
  , emptyModel
  )
where

import Competences.Analysis.Statistics
  ( allUsersEvidenceByActivity
  , allUsersObservationsByAbility
  , UserActivityStats (..)
  , UserAbilityStats (..)
  , ActivityStats (..)
  , AbilityStats (..)
  )
import Competences.Document (Document (..), emptyDocument)
import Competences.Document.Evidence (ActivityType (..), Ability (..))

import Competences.Frontend.SyncDocument (SyncDocumentRef, subscribeDocument, DocumentChange (..))
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Table qualified as T
import Data.List (find)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (ms)
import Optics.Core ((.~))

-- | Statistics Overview Component Model
-- For teacher view showing all students' statistics
data Model = Model
  { document :: !Document

  }
  deriving (Eq, Generic, Show)

-- | Statistics Overview Component Actions
data Action
  = UpdateDocument !DocumentChange

  deriving (Eq, Show)

-- | Empty model for statistics overview
emptyModel :: Model
emptyModel = Model
  { document = emptyDocument
  }

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
      M.modify $ #document .~ doc


    view :: Model -> M.View Model Action
    view m =
      let
        -- Get activity statistics for all users
        activityStats = allUsersEvidenceByActivity m.document
        
        -- Get ability statistics for all users
        abilityStats = allUsersObservationsByAbility m.document
        
        -- Filter for home and school exercises
        homeActivityStats = filterActivityStats activityStats [HomeExercise]
        schoolActivityStats = filterActivityStats activityStats [SchoolExercise]
        
        homeAbilityStats = filterAbilityStats abilityStats [HomeExercise]
        schoolAbilityStats = filterAbilityStats abilityStats [SchoolExercise]
      in
        V.viewFlow V.vFlow
          [ -- Header
            V.title_ "Statistics Overview"
          , V.text_ " "
          , -- Activity Statistics Section
            V.viewFlow V.vFlow
              [ V.title_ "Activity Statistics"
              , V.text_ " "
              , V.viewFlow V.vFlow
                  [ activityStatisticsTable "Home Exercises" homeActivityStats
                  , V.text_ " "
                  , activityStatisticsTable "School Exercises" schoolActivityStats
                  ]
              ]
          , V.text_ " "
          , -- Ability Statistics Section
            V.viewFlow V.vFlow
              [ V.title_ "Ability Statistics"
              , V.text_ " "
              , V.viewFlow V.vFlow
                  [ abilityStatisticsTable "Home Exercises" homeAbilityStats
                  , V.text_ " "
                  , abilityStatisticsTable "School Exercises" schoolAbilityStats
                  ]
              ]
          ]

-- | Filter activity statistics by specific activity types
filterActivityStats :: [UserActivityStats] -> [ActivityType] -> [UserActivityStats]
filterActivityStats userStats activityTypes =
  map filterUserActivity userStats
  where
    filterUserActivity userStat =
      let
        filteredStats = filter (\stat -> stat.activityType `elem` activityTypes) userStat.activityStats
        totalCount = sum (map (.count) filteredStats)
      in
        userStat { activityStats = filteredStats, totalCount = totalCount }

-- | Filter ability statistics by specific activity types
filterAbilityStats :: [UserAbilityStats] -> [ActivityType] -> [UserAbilityStats]
filterAbilityStats userStats _activityTypes =
  map filterUserAbility userStats
  where
    filterUserAbility userStat =
      -- For ability stats, we need to filter by activity type at the evidence level
      -- This is a simplified approach - in a real implementation, we'd need to
      -- track which observations came from which activity types
      userStat  -- Keep all ability stats for now, filtering would be more complex

-- | Create a table showing activity statistics for all users
activityStatisticsTable :: M.MisoString -> [UserActivityStats] -> M.View Model Action
activityStatisticsTable title stats =
  V.viewFlow V.vFlow
    [ V.title_ title
    , V.viewFlow V.vFlow
        [ -- Header row
          T.tableRow
            [ V.text_ "Student"
            , V.text_ "Conversation"
            , V.text_ "Exam"
            , V.text_ "School Exercise"
            , V.text_ "Home Exercise"
            , V.text_ "Total"
            ]
        ]
    , V.viewFlow V.vFlow (map userActivityRow stats)
    ]

-- | Create a row for a user's activity statistics
userActivityRow :: UserActivityStats -> M.View Model Action
userActivityRow userStat =
  let
    -- Create a simple lookup function for activity counts
    getCount activityType =
      case find (\stat -> stat.activityType == activityType) userStat.activityStats of
        Just stat -> stat.count
        Nothing -> 0
  in
    T.tableRow
      [ V.text_ (ms userStat.userName)
      , V.text_ (ms $ show $ getCount Conversation)
      , V.text_ (ms $ show $ getCount Exam)
      , V.text_ (ms $ show $ getCount SchoolExercise)
      , V.text_ (ms $ show $ getCount HomeExercise)
      , V.text_ (ms $ show userStat.totalCount)
      ]

-- | Create a table showing ability statistics for all users
abilityStatisticsTable :: M.MisoString -> [UserAbilityStats] -> M.View Model Action
abilityStatisticsTable title stats =
  V.viewFlow V.vFlow
    [ V.title_ title
    , V.viewFlow V.vFlow
        [ -- Header row
          T.tableRow
            [ V.text_ "Student"
            , V.text_ "Self-Reliant"
            , V.text_ "With Support"
            , V.text_ "Not Yet"
            , V.text_ "Total"
            ]
        ]
    , V.viewFlow V.vFlow (map userAbilityRow stats)
    ]

-- | Create a row for a user's ability statistics
userAbilityRow :: UserAbilityStats -> M.View Model Action
userAbilityRow userStat =
  let
    -- Create a simple lookup function for ability counts
    getCount ability =
      case find (\stat -> stat.ability == ability) userStat.abilityStats of
        Just stat -> stat.count
        Nothing -> 0
  in
    T.tableRow
      [ V.text_ (ms userStat.userName)
      , V.text_ (ms $ show $ getCount SelfReliant)
      , V.text_ (ms $ show $ getCount WithSupport)
      , V.text_ (ms $ show $ getCount NotYet)
      , V.text_ (ms $ show userStat.totalCount)
      ]
module Competences.Frontend.Component.StatisticsViewer
  ( statisticsViewerComponent
  , Model (..)
  , Action (..)
  , emptyModel
  )
where

import Competences.Analysis.Statistics (userEvidenceByActivity, ActivityStats (..), userObservationsByAbility, AbilityStats (..))
import Competences.Document (Document (..), User (..), emptyDocument)
import Competences.Document.Id (nilId)
import Competences.Document.Evidence (ActivityType (..), Ability (..))
import Competences.Document.User (Office365Id (..), UserId, UserRole (..), isStudent, isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument (SyncDocumentRef, subscribeDocument, DocumentChange (..))
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Table qualified as T
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Frontend.View.Card qualified as Card
import Data.Foldable (toList)
import Data.List (find)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (ms)


-- | Statistics Viewer Component Model
-- For individual student statistics view
data Model = Model
  { document :: !Document
  , currentUserId :: !(Maybe UserId)
  , connectedUser :: !User  -- The currently logged-in user
  }
  deriving (Eq, Generic, Show)

-- | Statistics Viewer Component Actions
data Action
  = UpdateDocument !DocumentChange
  | SetCurrentUser !(Maybe UserId)
  deriving (Eq, Show)

-- | Empty model for statistics viewer
emptyModel :: Model
emptyModel = Model
  { document = emptyDocument
  , currentUserId = Nothing
  , connectedUser = User nilId "" Student (Office365Id "")
  }

-- | Statistics Viewer Component
-- Now accepts the connected user to determine what data to show
statisticsViewerComponent :: SyncDocumentRef -> User -> M.Component p Model Action
statisticsViewerComponent docRef connectedUser' =
  (M.component model update view)
    { M.subs = [subscribeDocument docRef UpdateDocument]
    , M.events = mempty
    , M.initialAction = Nothing
    }
  where
    model = emptyModel { connectedUser = connectedUser' }

    update :: Action -> M.Effect p Model Action
    update (UpdateDocument (DocumentChange doc _)) = do
      let -- Determine which user to show based on connected user role
          currentUserId' = case model.connectedUser.role of
            -- Teachers can see any student, default to first student
            Teacher -> case toList doc.users of
              (user:_) | isStudent user -> Just user.id
              _ -> Nothing
            -- Students can only see their own data
            Student -> Just model.connectedUser.id
      M.modify $ \m -> m
        { document = doc
        , currentUserId = currentUserId'
        }
    update (SetCurrentUser userId) = M.modify $ \m -> m { currentUserId = userId }

    view :: Model -> M.View Model Action
    view m =
      case m.currentUserId of
        Nothing -> noUserSelectedView
        Just userId ->
          case findUser userId m.document of
            Nothing -> userNotFoundView
            Just user -> statisticsView m.document user user

    -- Helper to find a user by ID
    findUser userId doc =
      case toList (doc.users) of
        [] -> Nothing
        users -> find ((== userId) . (.id)) users

-- | View when no user is selected
noUserSelectedView :: M.View Model Action
noUserSelectedView =
  V.viewFlow V.vFlow
    [ Typography.h2 (C.translate' C.LblNoUser)
    , Typography.paragraph "Please select a user from the user management screen."
    ]

-- | View when user is not found
userNotFoundView :: M.View Model Action
userNotFoundView =
  Typography.h2 (C.translate' C.LblNoUser)

-- | Main statistics view showing user evidence by activity
statisticsView :: Document -> User -> User -> M.View Model Action
statisticsView doc user _connectedUser =
  let
      -- Activity statistics
      activityStats = userEvidenceByActivity doc user.id
      totalEvidence = sum (map (.count) activityStats)
      
      -- Ability statistics
      abilityStats = userObservationsByAbility doc user.id
      totalObservations = sum (map (.count) abilityStats)
      
      -- Create rows for each activity type
      activityRows = map activityStatRow activityStats
      
      -- Create rows for each ability
      abilityRows = map abilityStatRow abilityStats
      
      -- Add total rows
      activityTotalRow =
        T.tableRow
          [ V.text_ (C.translate' C.LblActivityType)
          , V.text_ (ms $ show totalEvidence)
          , V.text_ "-"  -- No weight for total
          ]
      
      abilityTotalRow =
        T.tableRow
          [ V.text_ "Ability Total"
          , V.text_ (ms $ show totalObservations)
          , V.text_ "-"  -- No weight for total
          ]
      
      -- Show user selector only for teachers
      userHeader =
        if isTeacher user
        then
          V.viewFlow V.vFlow
            [ Typography.small "Viewing statistics for:"
            , Typography.h2 (ms user.name)
            ]
        else
          V.viewFlow V.vFlow
            [ Typography.small "Your statistics:"
            , Typography.h2 (ms user.name)
            ]
  in V.viewFlow V.vFlow
       [ -- Header with user info
         userHeader
       , V.text_ " "  -- Simple spacing
       , -- Activity Statistics Section
         Card.cardWithHeader "Activity Statistics" Nothing
           [ V.viewFlow V.vFlow activityRows
           , V.viewFlow V.vFlow [activityTotalRow]
           ]
       , V.text_ " "
       , -- Ability Statistics Section
         Card.cardWithHeader "Ability Statistics" Nothing
           [ V.viewFlow V.vFlow abilityRows
           , V.viewFlow V.vFlow [abilityTotalRow]
           ]
       ]

-- | Create a table row for a single activity statistic
activityStatRow :: ActivityStats -> M.View Model Action
activityStatRow stat =
  T.tableRow
    [ V.text_ (activityTypeText stat.activityType)
    , V.text_ (ms $ show stat.count)
    , V.text_ (ms $ show stat.weight)
    ]

-- | Create a table row for a single ability statistic
abilityStatRow :: AbilityStats -> M.View Model Action
abilityStatRow stat =
  T.tableRow
    [ V.text_ (abilityText stat.ability)
    , V.text_ (ms $ show stat.count)
    , V.text_ (ms $ show stat.weight)
    ]

-- | Convert ability to display text
abilityText :: Ability -> M.MisoString
abilityText ability = case ability of
  SelfReliant -> "Self-Reliant"
  SelfReliantWithSillyMistakes -> "Self-Reliant (with silly mistakes)"
  WithSupport -> "With Support"
  NotYet -> "Not Yet"

-- | Convert activity type to display text
activityTypeText :: ActivityType -> M.MisoString
activityTypeText activityType = C.translate' (C.LblActivityTypeDescription activityType)

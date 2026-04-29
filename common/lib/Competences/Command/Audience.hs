{-# LANGUAGE CPP #-}

module Competences.Command.Audience
  ( CommandAudience (..)
  , audienceToText
  , audienceFromText
  , audienceRecipients
  )
where

import Competences.Document.User (UserId)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Audience classification for a command.
--
-- Single source of truth for "who needs to see this command". Computed by
-- 'handleCommand' alongside the resulting document, then used both for live
-- WebSocket fan-out ('CommandProcessor') and DB persistence
-- ('saveCommandWithAudience') so a reconnecting client can be served only
-- the commands relevant to it via 'loadCommandsForUser'.
data CommandAudience
  = -- | Affects all users (structural changes: competences, grids, tasks, users, etc.)
    AudienceAll
  | -- | Affects only teachers (meso plans, lessons)
    AudienceTeachers
  | -- | Affects all teachers plus specific students
    AudienceTeachersAnd ![UserId]
  | -- | Affects only specific users
    AudienceOnly ![UserId]
  deriving (Eq, Generic, Show)

instance Binary CommandAudience

#ifdef WITH_AESON
instance FromJSON CommandAudience
instance ToJSON CommandAudience
#endif

-- | Combine two audiences as a union — needed when a Modify command's
-- pre-state and post-state audiences differ (e.g. an Assignment is
-- reassigned from student A to student B; both must see the update).
instance Semigroup CommandAudience where
  AudienceAll <> _ = AudienceAll
  _ <> AudienceAll = AudienceAll
  AudienceTeachers <> AudienceTeachers = AudienceTeachers
  AudienceTeachers <> AudienceTeachersAnd xs = AudienceTeachersAnd xs
  AudienceTeachersAnd xs <> AudienceTeachers = AudienceTeachersAnd xs
  AudienceTeachersAnd xs <> AudienceTeachersAnd ys = AudienceTeachersAnd (dedup (xs <> ys))
  AudienceTeachers <> AudienceOnly xs = AudienceTeachersAnd xs
  AudienceOnly xs <> AudienceTeachers = AudienceTeachersAnd xs
  AudienceTeachersAnd xs <> AudienceOnly ys = AudienceTeachersAnd (dedup (xs <> ys))
  AudienceOnly xs <> AudienceTeachersAnd ys = AudienceTeachersAnd (dedup (xs <> ys))
  AudienceOnly xs <> AudienceOnly ys = AudienceOnly (dedup (xs <> ys))

dedup :: (Ord a) => [a] -> [a]
dedup = Set.toList . Set.fromList

-- | Convert audience to the text representation stored in the database.
audienceToText :: CommandAudience -> Text
audienceToText AudienceAll = "all"
audienceToText AudienceTeachers = "teachers"
audienceToText (AudienceTeachersAnd _) = "teachers_and_recipients"
audienceToText (AudienceOnly _) = "recipients"

-- | Parse audience from the text representation stored in the database.
-- Recipients must be supplied separately (from the command_recipients table).
audienceFromText :: Text -> [UserId] -> CommandAudience
audienceFromText "all" _ = AudienceAll
audienceFromText "teachers" _ = AudienceTeachers
audienceFromText "teachers_and_recipients" rs = AudienceTeachersAnd rs
audienceFromText "recipients" rs = AudienceOnly rs
audienceFromText _ _ = AudienceAll -- fallback

-- | Extract the specific recipient user IDs from an audience (if any).
-- Returns the list of user IDs that need entries in the command_recipients table.
audienceRecipients :: CommandAudience -> [UserId]
audienceRecipients AudienceAll = []
audienceRecipients AudienceTeachers = []
audienceRecipients (AudienceTeachersAnd uids) = uids
audienceRecipients (AudienceOnly uids) = uids

module Competences.Document.Resource
  ( -- * IDs
    ResourceId
    -- * Identifier
  , ResourceIdentifier (..)
    -- * Content Types
  , ResourceContent (..)
    -- * Resource
  , Resource (..)
  , ResourceIxs
  , mkResource
  )
where

import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Id (Id)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import Competences.TaskContent.RichContent (RichContent)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | ID for a Resource.
type ResourceId = Id Resource

-- | Human-readable identifier for a Resource.
-- Non-unique (e.g., "Book p.42", "Video 1.2").
newtype ResourceIdentifier = ResourceIdentifier Text
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (Binary, FromJSON, ToJSON)

-- | Content type for a resource. Exactly one of:
-- - Inline content (rich text with MathJax)
-- - Web link (URL + description)
-- - Video link (URL + description)
data ResourceContent
  = -- | Rich text content with MathJax support
    InlineContent !RichContent
  | -- | Web link with URL and description
    WebLink !Text !Text
  | -- | Video link with URL and description
    VideoLink !Text !Text
  deriving (Eq, Generic, Ord, Show)

instance FromJSON ResourceContent where
  parseJSON = withObject "ResourceContent" $ \v -> do
    tag <- v .: "tag"
    case tag :: Text of
      "InlineContent" -> InlineContent <$> v .: "content"
      "WebLink" -> WebLink <$> v .: "url" <*> v .: "description"
      "VideoLink" -> VideoLink <$> v .: "url" <*> v .: "description"
      _ -> fail "Invalid ResourceContent tag"

instance ToJSON ResourceContent where
  toJSON (InlineContent content) =
    object
      [ "tag" .= ("InlineContent" :: Text)
      , "content" .= content
      ]
  toJSON (WebLink url desc) =
    object
      [ "tag" .= ("WebLink" :: Text)
      , "url" .= url
      , "description" .= desc
      ]
  toJSON (VideoLink url desc) =
    object
      [ "tag" .= ("VideoLink" :: Text)
      , "url" .= url
      , "description" .= desc
      ]

instance Binary ResourceContent

-- | A learning resource associated with competence levels.
data Resource = Resource
  { id :: !ResourceId
  , identifier :: !ResourceIdentifier
    -- ^ Human-readable identifier (e.g., "Book p.42", "Video 1.2").
    -- Non-unique, used for display purposes.
  , competenceLevels :: ![CompetenceLevelId]
    -- ^ List of competence levels this resource is useful for.
    -- Multiple levels can be associated with a single resource.
  , content :: !ResourceContent
    -- ^ The resource content (inline, web link, or video link).
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON Resource where
  parseJSON = withObject "Resource" $ \v ->
    Resource
      <$> v .: "id"
      <*> v .: "identifier"
      <*> v .: "competenceLevels"
      <*> v .: "content"

instance ToJSON Resource where
  toJSON resource =
    object
      [ "id" .= resource.id
      , "identifier" .= resource.identifier
      , "competenceLevels" .= resource.competenceLevels
      , "content" .= resource.content
      ]

instance Binary Resource

-- | IxSet indices for Resource.
-- Indexed by ResourceId (unique) and CompetenceLevelId (multi-value for efficient lookup).
type ResourceIxs = '[ResourceId, CompetenceLevelId]

instance Ix.Indexable ResourceIxs Resource where
  indices =
    Ix.ixList
      (Ix.ixFun $ \r -> [r.id])
      (Ix.ixFun (.competenceLevels))  -- Multi-value index

-- | Create a new empty resource with the given ID.
mkResource :: ResourceId -> Resource
mkResource rid = Resource
  { id = rid
  , identifier = ResourceIdentifier ""
  , competenceLevels = []
  , content = InlineContent mempty
  }

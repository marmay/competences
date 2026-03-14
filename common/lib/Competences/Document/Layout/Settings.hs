{-# LANGUAGE CPP #-}

module Competences.Document.Layout.Settings
  ( -- * Print settings
    PaperSize (..)
  , Orientation (..)
  , TaskHeaderStyle (..)
  , TaskLayout (..)
  , GridConfig (..)
  , PrintSettings (..)
  , defaultPrintSettings
    -- * Content settings
  , TaskContentSetting (..)
  , ContentSettings (..)
  , ContentPreset (..)
  )
where

#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Competences.Document.Id (Id)
import Competences.Document.Solution (Solution)
import Competences.Document.Task (TaskId)
import GHC.Generics (Generic)

-- | Supported paper sizes
data PaperSize = A4 | A5
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

instance Binary PaperSize

#ifdef WITH_AESON
instance FromJSON PaperSize

instance ToJSON PaperSize
#endif

-- | Page orientation
data Orientation = Portrait | Landscape
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

instance Binary Orientation

#ifdef WITH_AESON
instance FromJSON Orientation

instance ToJSON Orientation
#endif

-- | How the task header (h2) is rendered
data TaskHeaderStyle = HeaderNumber | HeaderTitle | HeaderBoth
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

instance Binary TaskHeaderStyle

#ifdef WITH_AESON
instance FromJSON TaskHeaderStyle

instance ToJSON TaskHeaderStyle
#endif

-- | How tasks are laid out on the page
data TaskLayout
  = -- | Tasks flow continuously, no forced page breaks
    Continuous
  | -- | Tasks arranged in a grid (rows x cols per page)
    Grid !GridConfig
  deriving (Eq, Ord, Show, Generic)

instance Binary TaskLayout

#ifdef WITH_AESON
instance FromJSON TaskLayout

instance ToJSON TaskLayout
#endif

-- | Grid dimensions (rows and columns per page)
data GridConfig = GridConfig
  { rows :: !Int
  , cols :: !Int
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary GridConfig

#ifdef WITH_AESON
instance FromJSON GridConfig

instance ToJSON GridConfig
#endif

-- | Print configuration
data PrintSettings = PrintSettings
  { paperSize :: !PaperSize
  , orientation :: !Orientation
  , baseFontSize :: !Double
  , taskLayout :: !TaskLayout
  , groupedCopies :: !Int
  , totalCopies :: !Int
  , showTitle :: !Bool
  , showHeader :: !Bool
  , showFooter :: !Bool
  , showNameField :: !Bool
  , taskHeaderStyle :: !TaskHeaderStyle
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary PrintSettings

#ifdef WITH_AESON
instance FromJSON PrintSettings

instance ToJSON PrintSettings
#endif

-- | Default: A4 portrait, continuous layout, 1 copy each
defaultPrintSettings :: PrintSettings
defaultPrintSettings =
  PrintSettings
    { paperSize = A4
    , orientation = Portrait
    , baseFontSize = 10.0
    , taskLayout = Continuous
    , groupedCopies = 1
    , totalCopies = 1
    , showTitle = True
    , showHeader = True
    , showFooter = True
    , showNameField = True
    , taskHeaderStyle = HeaderBoth
    }

-- | Per-task content settings for print output
data TaskContentSetting = TaskContentSetting
  { showDescription :: !Bool
  , visibleSolutions :: !(Set (Id Solution))
  , gridHeightMm :: !(Maybe Double)
  , inlineAnswer :: !Bool
  , itemsPerRow :: !Int
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary TaskContentSetting

#ifdef WITH_AESON
instance FromJSON TaskContentSetting

instance ToJSON TaskContentSetting
#endif

-- | Content settings: per-task map of what to include
newtype ContentSettings = ContentSettings
  { perTask :: Map TaskId TaskContentSetting
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary ContentSettings

#ifdef WITH_AESON
instance FromJSON ContentSettings

instance ToJSON ContentSettings
#endif

-- | Preset configurations for quick setup
data ContentPreset
  = Aufgabenblatt
  | Arbeitsblatt
  | Loesungsblatt
  | Musteraufgaben
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

instance Binary ContentPreset

#ifdef WITH_AESON
instance FromJSON ContentPreset

instance ToJSON ContentPreset
#endif

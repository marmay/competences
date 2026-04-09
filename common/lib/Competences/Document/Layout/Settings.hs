{-# LANGUAGE CPP #-}

module Competences.Document.Layout.Settings
  ( -- * Print settings
    PaperSize (..)
  , Orientation (..)
  , TaskHeaderStyle (..)
  , TaskLayout (..)
  , GridConfig (..)
  , FontFamily (..)
  , PrintSettings (..)
  , defaultPrintSettings
    -- * Content settings
  , TaskContentSetting (..)
  , ContentSettings (..)
  , defaultContentSettings
  , ContentPreset (..)
    -- * Per-image print settings
  , PrintImagePosition (..)
  , ImagePrintSetting (..)
  , defaultImagePrintSetting
  )
where

#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.!=), withObject)
import Data.Map.Strict qualified as Map
#endif
import Data.Binary (Binary)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
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

-- | Font family for print output
data FontFamily = DefaultFont | IwonaFont
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

instance Binary FontFamily

#ifdef WITH_AESON
instance FromJSON FontFamily

instance ToJSON FontFamily
#endif

-- | Print configuration (layout-only concerns)
data PrintSettings = PrintSettings
  { paperSize :: !PaperSize
  , orientation :: !Orientation
  , baseFontSize :: !Double
  , taskLayout :: !TaskLayout
  , groupedCopies :: !Int
  , totalCopies :: !Int
  , showHeader :: !Bool
  , showFooter :: !Bool
  , taskHeaderStyle :: !TaskHeaderStyle
  , duplexLayout :: !Bool
  , distributeLastPage :: !Bool
  , fontFamily :: !FontFamily
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary PrintSettings

#ifdef WITH_AESON
-- | Custom FromJSON: consumes and discards old showTitle/showNameField keys,
-- uses defaults for new fields.
instance FromJSON PrintSettings where
  parseJSON = withObject "PrintSettings" $ \v -> do
    ps <- v .: "paperSize"
    orient <- v .: "orientation"
    bfs <- v .: "baseFontSize"
    tl <- v .: "taskLayout"
    gc <- v .: "groupedCopies"
    tc <- v .: "totalCopies"
    -- Consume old fields silently (backward compat)
    _ <- v .:? "showTitle" .!= (True :: Bool)
    _ <- v .:? "showNameField" .!= (True :: Bool)
    sh <- v .: "showHeader"
    sf <- v .: "showFooter"
    ths <- v .: "taskHeaderStyle"
    dl <- v .:? "duplexLayout" .!= False
    dlp <- v .:? "distributeLastPage" .!= True
    ff <- v .:? "fontFamily" .!= DefaultFont
    pure PrintSettings
      { paperSize = ps
      , orientation = orient
      , baseFontSize = bfs
      , taskLayout = tl
      , groupedCopies = gc
      , totalCopies = tc
      , showHeader = sh
      , showFooter = sf
      , taskHeaderStyle = ths
      , duplexLayout = dl
      , distributeLastPage = dlp
      , fontFamily = ff
      }

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
    , showHeader = True
    , showFooter = True
    , taskHeaderStyle = HeaderBoth
    , duplexLayout = False
    , distributeLastPage = True
    , fontFamily = DefaultFont
    }

-- | Position of an image in print output
data PrintImagePosition
  = PrintInline -- ^ Centered in content flow (default, same as screen)
  | PrintFloatRight -- ^ Float right at image's position in markdown
  | PrintFloatTop -- ^ Float right, extracted to top of task (before header)
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

instance Binary PrintImagePosition

#ifdef WITH_AESON
instance FromJSON PrintImagePosition
instance ToJSON PrintImagePosition
#endif

-- | Per-image print settings
data ImagePrintSetting = ImagePrintSetting
  { sizePct :: !Int -- ^ 10..100, step 5. Controls max-width percentage.
  , position :: !PrintImagePosition -- ^ Where the image renders
  , backdrop :: !Bool -- ^ White background behind image (for transparent SVGs)
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary ImagePrintSetting

#ifdef WITH_AESON
instance FromJSON ImagePrintSetting
instance ToJSON ImagePrintSetting
#endif

-- | Default: inline, full size, no backdrop
defaultImagePrintSetting :: ImagePrintSetting
defaultImagePrintSetting =
  ImagePrintSetting
    { sizePct = 100
    , position = PrintInline
    , backdrop = False
    }

-- | Per-task content settings for print output
data TaskContentSetting = TaskContentSetting
  { showDescription :: !Bool
  , visibleSolutions :: !(Set (Id Solution))
  , gridHeightMm :: !(Maybe Double)
  , inlineAnswer :: !Bool
  , itemsPerRow :: !Int
  , points :: !(Maybe Double)
  , imageSettings :: !(Map Text ImagePrintSetting)
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary TaskContentSetting

#ifdef WITH_AESON
-- | Custom FromJSON: backward compat for points and imageSettings
instance FromJSON TaskContentSetting where
  parseJSON = withObject "TaskContentSetting" $ \v -> do
    sd <- v .: "showDescription"
    vs <- v .: "visibleSolutions"
    ghm <- v .: "gridHeightMm"
    ia <- v .: "inlineAnswer"
    ipr <- v .: "itemsPerRow"
    pts <- v .:? "points" .!= Nothing
    ims <- v .:? "imageSettings" .!= Map.empty
    pure TaskContentSetting
      { showDescription = sd
      , visibleSolutions = vs
      , gridHeightMm = ghm
      , inlineAnswer = ia
      , itemsPerRow = ipr
      , points = pts
      , imageSettings = ims
      }

instance ToJSON TaskContentSetting
#endif

-- | Content settings: per-task map + global content options
data ContentSettings = ContentSettings
  { perTask :: !(Map TaskId TaskContentSetting)
  , showTitle :: !Bool
  , showNameField :: !Bool
  , customFooter :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary ContentSettings

#ifdef WITH_AESON
-- | Custom FromJSON: old JSON is {"perTask": {...}}, new fields use defaults
instance FromJSON ContentSettings where
  parseJSON = withObject "ContentSettings" $ \v -> do
    pt <- v .: "perTask"
    st <- v .:? "showTitle" .!= True
    snf <- v .:? "showNameField" .!= True
    cf <- v .:? "customFooter" .!= Nothing
    pure ContentSettings
      { perTask = pt
      , showTitle = st
      , showNameField = snf
      , customFooter = cf
      }

instance ToJSON ContentSettings
#endif

-- | Default content settings
defaultContentSettings :: ContentSettings
defaultContentSettings =
  ContentSettings
    { perTask = mempty
    , showTitle = True
    , showNameField = True
    , customFooter = Nothing
    }

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

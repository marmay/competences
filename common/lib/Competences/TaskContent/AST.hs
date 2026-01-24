{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.TaskContent.AST
-- Description : AST for task content markup language
--
-- A minimal markup language for task content supporting:
--
-- * Paragraphs (blank-line separated)
-- * @*emph*@ for emphasis (italic)
-- * @**strong**@ for bold
-- * Sub tasks: @a.@, @b.@, @c.@ (lettered lists)
-- * Sub questions: @1.@, @2.@, @3.@ (numbered lists)
-- * Embedded MathJax: @$...$@ (inline) and @$$...$$@ (block)
module Competences.TaskContent.AST
  ( -- * Document structure
    TaskContent (..)
  , Block (..)
  , ListItem (..)
  , Inline (..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Root document type - a sequence of blocks
newtype TaskContent = TaskContent [Block]
  deriving (Eq, Show, Generic)

instance ToJSON TaskContent
instance FromJSON TaskContent

-- | Block-level elements
data Block
  = -- | Regular paragraph containing inline elements
    Paragraph ![Inline]
  | -- | Lettered list for sub-tasks (a., b., c.)
    SubTaskList ![ListItem]
  | -- | Numbered list for sub-questions (1., 2., 3.)
    SubQuestionList ![ListItem]
  | -- | Display math block ($$...$$ or \[...\])
    MathBlock !Text
  | -- | Heading with level (1-6) and inline content
    Heading !Int ![Inline]
  deriving (Eq, Show, Generic)

instance ToJSON Block
instance FromJSON Block

-- | List item for both subtasks and subquestions
-- Content can span multiple lines/paragraphs using indentation
data ListItem = ListItem
  { marker :: !Text
  -- ^ The marker text (e.g., "a." or "1.")
  , content :: ![Block]
  -- ^ Item content as block elements (allows multiple paragraphs, math blocks, etc.)
  }
  deriving (Eq, Show, Generic)

instance ToJSON ListItem
instance FromJSON ListItem

-- | Inline elements within paragraphs and list items
data Inline
  = -- | Regular text
    Plain !Text
  | -- | Emphasized text (*...*)
    Emph ![Inline]
  | -- | Strong/bold text (**...**)
    Strong ![Inline]
  | -- | Inline math ($...$)
    MathInline !Text
  deriving (Eq, Show, Generic)

instance ToJSON Inline
instance FromJSON Inline

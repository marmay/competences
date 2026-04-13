-- | Shared lenses for Task/TaskPatch fields used by both
-- TaskDetailView (inline editor) and TaskPinEditor (pin editor).
module Competences.Frontend.Component.TaskEditor.Lenses
  ( identifierViewLens
  , identifierPatchLens
  , titleViewLens
  , titlePatchLens
  , contentViewLens
  , contentPatchLens
  )
where

import Competences.Command (TaskPatch (..))
import Competences.Command.Common (Change)
import Competences.Document (Task (..))
import Competences.Document.Task (TaskIdentifier (..))
import Competences.TaskContent.RichContent (RichContent)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Optics.Core (Iso', Lens', iso, (%))

-- Lenses for identifier (TaskIdentifier <-> Text conversion)
taskIdentifierTextIso :: Iso' TaskIdentifier Text
taskIdentifierTextIso = iso (\(TaskIdentifier t) -> t) TaskIdentifier

changeTaskIdentifierTextIso :: Iso' (Change TaskIdentifier) (Change Text)
changeTaskIdentifierTextIso = iso fwd bwd
  where
    fwd Nothing = Nothing
    fwd (Just (TaskIdentifier a, TaskIdentifier b)) = Just (a, b)
    bwd Nothing = Nothing
    bwd (Just (a, b)) = Just (TaskIdentifier a, TaskIdentifier b)

identifierViewLens :: Lens' Task Text
identifierViewLens = #identifier % taskIdentifierTextIso

identifierPatchLens :: Lens' TaskPatch (Change Text)
identifierPatchLens = #identifier % changeTaskIdentifierTextIso

-- Lenses for title (plain Text field)
titleViewLens :: Lens' Task Text
titleViewLens = #title

titlePatchLens :: Lens' TaskPatch (Change Text)
titlePatchLens = #title

-- Lenses for content (Maybe RichContent <-> RichContent conversion, empty = Nothing)
contentIso :: Iso' (Maybe RichContent) RichContent
contentIso = iso (fromMaybe mempty) (\t -> if t == mempty then Nothing else Just t)

changeContentIso :: Iso' (Change (Maybe RichContent)) (Change RichContent)
changeContentIso = iso fwd bwd
  where
    fwd Nothing = Nothing
    fwd (Just (a, b)) = Just (fromMaybe mempty a, fromMaybe mempty b)
    bwd Nothing = Nothing
    bwd (Just (a, b)) = Just (if a == mempty then Nothing else Just a, if b == mempty then Nothing else Just b)

contentViewLens :: Lens' Task RichContent
contentViewLens = #content % contentIso

contentPatchLens :: Lens' TaskPatch (Change RichContent)
contentPatchLens = #content % changeContentIso

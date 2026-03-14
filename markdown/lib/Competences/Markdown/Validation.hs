-- |
-- Module      : Competences.Markdown.Validation
-- Description : Validate markdown with embedded geometry blocks
--
-- Parses markdown and then validates any embedded geometry code blocks
-- (version check + parse check). Returns all errors with context labels
-- so the editor can display them.
module Competences.Markdown.Validation
  ( ValidationError (..)
  , validateMarkdown
  )
where

import Competences.Markdown.AST qualified as MD
import Competences.Markdown.Geometry.Parser
  ( currentGeometryVersion
  , geometryVersionText
  , isGeometryInfo
  , parseGeometry
  , parseGeometryVersion
  )
import Competences.Markdown.Parser qualified as Markdown
import Data.Text (Text)
import Data.Text qualified as T

-- | A validation error with context (e.g. "Geometrie-Block 1") and message.
data ValidationError = ValidationError
  { context :: !Text
  , message :: !Text
  }
  deriving (Eq, Show)

-- | Validate markdown text: check markdown syntax, then validate all
-- embedded geometry blocks for version compatibility and parse correctness.
validateMarkdown :: Text -> [ValidationError]
validateMarkdown txt =
  case Markdown.parseMarkdown txt of
    Left err ->
      [ ValidationError
          { context = "Markdown"
          , message = Markdown.formatParseError err
          }
      ]
    Right (MD.Document blocks) ->
      validateBlocks 1 blocks

-- | Walk blocks, validating geometry code blocks.
-- The counter tracks the geometry block number for context labels.
validateBlocks :: Int -> [MD.Block] -> [ValidationError]
validateBlocks !n = \case
  [] -> []
  MD.FencedCodeBlock (Just info) body : rest
    | isGeometryInfo info ->
        validateGeometryBlock n info body ++ validateBlocks (n + 1) rest
  MD.Admonition _ _ bodyBlocks : rest ->
    let innerErrors = validateBlocks n bodyBlocks
        nextN = n + countGeometryBlocks bodyBlocks
     in innerErrors ++ validateBlocks nextN rest
  MD.OrderedList _ items : rest ->
    let (errors, nextN) = validateItems n items
     in errors ++ validateBlocks nextN rest
  MD.BulletList items : rest ->
    let (errors, nextN) = validateItems n items
     in errors ++ validateBlocks nextN rest
  MD.LetterList items : rest ->
    let (errors, nextN) = validateItems n items
     in errors ++ validateBlocks nextN rest
  MD.NotesGrid c1 c2 c3 c4 : rest ->
    let innerErrors = concatMap (validateBlocks n) [c1, c2, c3, c4]
        nextN = n + sum (map countGeometryBlocks [c1, c2, c3, c4])
     in innerErrors ++ validateBlocks nextN rest
  MD.ClozeBlock body opts : rest ->
    let bodyErrors = validateBlocks n body
        optBlocks = case opts of
          MD.ClozeNoOptions -> []
          MD.ClozeWordBank bs -> bs
          MD.ClozePerBlankOptions groups -> concat groups
        optErrors = validateBlocks (n + countGeometryBlocks body) optBlocks
        nextN = n + countGeometryBlocks body + countGeometryBlocks optBlocks
     in bodyErrors ++ optErrors ++ validateBlocks nextN rest
  MD.ChoiceBlock _ items : rest ->
    let (errors, nextN) = validateItems n items
     in errors ++ validateBlocks nextN rest
  MD.MappingBlock leftItems rightItems : rest ->
    let (leftErrors, midN) = validateItems n leftItems
        (rightErrors, nextN) = validateItems midN rightItems
     in leftErrors ++ rightErrors ++ validateBlocks nextN rest
  _ : rest -> validateBlocks n rest

-- | Validate list items (each item is [Block]).
validateItems :: Int -> [[MD.Block]] -> ([ValidationError], Int)
validateItems n [] = ([], n)
validateItems n (item : items) =
  let errs = validateBlocks n item
      nextN = n + countGeometryBlocks item
      (restErrs, finalN) = validateItems nextN items
   in (errs ++ restErrs, finalN)

-- | Count geometry blocks in a list of blocks (for counter tracking).
countGeometryBlocks :: [MD.Block] -> Int
countGeometryBlocks = \case
  [] -> 0
  MD.FencedCodeBlock (Just info) _ : rest
    | isGeometryInfo info -> 1 + countGeometryBlocks rest
  MD.Admonition _ _ bodyBlocks : rest ->
    countGeometryBlocks bodyBlocks + countGeometryBlocks rest
  MD.OrderedList _ items : rest ->
    sum (map countGeometryBlocks items) + countGeometryBlocks rest
  MD.BulletList items : rest ->
    sum (map countGeometryBlocks items) + countGeometryBlocks rest
  MD.LetterList items : rest ->
    sum (map countGeometryBlocks items) + countGeometryBlocks rest
  MD.NotesGrid c1 c2 c3 c4 : rest ->
    sum (map countGeometryBlocks [c1, c2, c3, c4]) + countGeometryBlocks rest
  MD.ClozeBlock body opts : rest ->
    let optBlocks = case opts of
          MD.ClozeNoOptions -> []
          MD.ClozeWordBank bs -> bs
          MD.ClozePerBlankOptions groups -> concat groups
     in countGeometryBlocks body + countGeometryBlocks optBlocks + countGeometryBlocks rest
  MD.ChoiceBlock _ items : rest ->
    sum (map countGeometryBlocks items) + countGeometryBlocks rest
  MD.MappingBlock leftItems rightItems : rest ->
    sum (map countGeometryBlocks leftItems) + sum (map countGeometryBlocks rightItems) + countGeometryBlocks rest
  _ : rest -> countGeometryBlocks rest

-- | Validate a single geometry block: check version, then parse body.
validateGeometryBlock :: Int -> Text -> Text -> [ValidationError]
validateGeometryBlock n info body =
  let ctx = "Geometrie-Block " <> T.pack (show n)
   in case geometryVersionText info of
        Nothing -> validateGeometryParse ctx body
        Just vText -> case parseGeometryVersion vText of
          Nothing ->
            [ ValidationError ctx ("Unbekannte Versionsangabe: " <> vText)
            ]
          Just (maj, _min)
            | maj > fst currentGeometryVersion ->
                [ ValidationError ctx $
                    "Benötigt Version " <> vText
                      <> ", aber nur V"
                      <> T.pack (show (fst currentGeometryVersion))
                      <> "."
                      <> T.pack (show (snd currentGeometryVersion))
                      <> " wird unterstützt."
                ]
            | otherwise -> validateGeometryParse ctx body

-- | Parse geometry body and return errors if any.
validateGeometryParse :: Text -> Text -> [ValidationError]
validateGeometryParse ctx body =
  case parseGeometry body of
    Right _ -> []
    Left err ->
      [ ValidationError ctx (T.pack $ show err)
      ]

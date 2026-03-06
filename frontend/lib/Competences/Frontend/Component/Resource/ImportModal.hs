{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.Resource.ImportModal
-- Description : Modal component for importing resources
--
-- Provides a modal dialog for importing resources from a markdown-like
-- format. Shows a preview of changes before applying.
module Competences.Frontend.Component.Resource.ImportModal
  ( resourceImportModalComponent
  , openResourceImportModal
  , Action
  )
where

import Competences.Command qualified as Cmd
import Competences.Command (ModifyCommand (..), ResourcePatch (..))
import Competences.Document (Document (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.FileRef (FileRef (..))
import Competences.Document.Resource (Resource (..), ResourceContent (..), ResourceIdentifier (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.ImportModal qualified as IM
import Competences.Frontend.SyncContext
  ( SyncContext (..)
  , modifySyncDocument
  , nextId
  )
import Competences.Frontend.SyncContext.WindowManager (ModalConfig (..), ModalId (..), ModalHeight (..), ModalWidth (..), WindowChrome (..), WindowMode, closeWindow, openFramedModalWith)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Import.Matching (matchResourceImport)
import Competences.Import.ResourceParser (parseResourceImport)
import Competences.Import.Types
  ( CompetenceMatch (..)
  , ImportAction (..)
  , ResourceImportPreview (..)
  , levelToGerman
  )
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M
import Miso.Html qualified as M

-- ============================================================================
-- Types (re-exports from generic module)
-- ============================================================================

type Action = IM.Action

-- ============================================================================
-- Component
-- ============================================================================

-- | Open the resource import modal as a framed modal.
openResourceImportModal :: SyncContext -> IO ()
openResourceImportModal r =
  let cfg = ModalConfig (WindowChrome (C.translate' C.LblImportResources) Icon.IcnImport) (ModalId "import-resources") ModalWide ModalFull Nothing
   in openFramedModalWith r.windowManager cfg (resourceImportModalComponent r)

resourceImportModalComponent :: SyncContext -> WindowMode -> M.Component p (IM.Model ResourceImportPreview) Action
resourceImportModalComponent = IM.importModalComponent resourceImportConfig

resourceImportConfig :: IM.ImportModalConfig ResourceImportPreview
resourceImportConfig =
  IM.ImportModalConfig
    { parse = \doc input -> case parseResourceImport input of
        Left err -> Left err
        Right parsed -> Right $ matchResourceImport doc parsed
    , renderItem = previewResourceView
    , hasChanges = resourceHasChanges
    , apply = applyResourceImport
    , placeholder =
        "# Buch S.42\n\n\
        \## Inhalt\n\
        \Beschreibung des Materials...\n\n\
        \## Kompetenzen\n\
        \- Rastername / Kompetenz / Wesentlich"
    }

-- ============================================================================
-- Preview View
-- ============================================================================

previewResourceView :: ResourceImportPreview -> M.View (IM.Model ResourceImportPreview) IM.Action
previewResourceView preview =
  M.div_
    [class_ "border border-border rounded-md p-3"]
    [ -- Resource header
      M.div_
        [class_ "mb-2"]
        [ Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
            [ M.span_ [class_ "font-semibold"] [M.text $ M.ms $ resourceName preview.resourceAction]
            , IM.actionBadge preview.resourceAction
            ]
        ]
    , -- Content preview
      M.div_
        [class_ "text-sm text-muted-foreground mb-2"]
        [M.text $ M.ms $ contentPreview preview.resourceAction]
    , -- Competence matches
      if null preview.competenceMatches
        then M.text ""
        else
          M.div_
            [class_ "mt-1 space-y-1"]
            (map competenceMatchView preview.competenceMatches)
    ]

resourceName :: ImportAction Resource -> Text
resourceName (Create r) = let ResourceIdentifier ident = r.identifier in ident
resourceName (Update _ r) = let ResourceIdentifier ident = r.identifier in ident
resourceName (NoChange r) = let ResourceIdentifier ident = r.identifier in ident

contentPreview :: ImportAction Resource -> Text
contentPreview action =
  let r = case action of
        Create x -> x
        Update _ x -> x
        NoChange x -> x
   in case r.content of
        InlineContent _ -> "Textinhalt"
        WebLink url _ -> "Link: " <> url
        VideoLink url _ -> "Video: " <> url
        FileContent fileRef -> "Datei: " <> fileRef.fileName

competenceMatchView :: CompetenceMatch -> M.View (IM.Model ResourceImportPreview) IM.Action
competenceMatchView cm =
  M.div_
    [class_ "flex items-center gap-1 text-xs"]
    [ M.span_ [class_ "text-muted-foreground"] [M.text $ M.ms cm.gridName]
    , M.span_ [] [M.text "/"]
    , M.span_ [] [M.text $ M.ms $ T.take 20 cm.description <> if T.length cm.description > 20 then "..." else ""]
    , Badge.outline (Badge.badgeText $ M.ms $ levelToGerman cm.level)
    , case cm.matched of
        Just _ -> Badge.primary (Badge.badgeText "OK")
        Nothing -> Badge.destructive (Badge.badgeText "?")
    ]

-- ============================================================================
-- Change Detection
-- ============================================================================

resourceHasChanges :: ResourceImportPreview -> Bool
resourceHasChanges preview = isChange preview.resourceAction

isChange :: ImportAction a -> Bool
isChange (Create _) = True
isChange (Update _ _) = True
isChange (NoChange _) = False

-- ============================================================================
-- Apply Import
-- ============================================================================

applyResourceImport :: SyncContext -> WindowMode -> Document -> [ResourceImportPreview] -> IO ()
applyResourceImport r wm _doc previews = do
  mapM_ (applyResourcePreview r) previews
  closeWindow wm

applyResourcePreview :: SyncContext -> ResourceImportPreview -> IO ()
applyResourcePreview r preview = do
  let matchedCompetences = mapMaybe (.matched) preview.competenceMatches

  case preview.resourceAction of
    Create res -> do
      newId <- nextId r
      let newResource =
            Resource
              { id = newId
              , identifier = res.identifier
              , competenceLevels = matchedCompetences
              , content = res.content
              , attachments = []
              }
      modifySyncDocument r (Cmd.Resources $ Cmd.OnResources $ Cmd.Create newResource)
    Update old new -> do
      modifySyncDocument r (Cmd.Resources $ Cmd.OnResources $ Cmd.Modify old.id Lock)
      let patch = buildResourcePatch old new matchedCompetences
      modifySyncDocument r (Cmd.Resources $ Cmd.OnResources $ Cmd.Modify old.id (Release patch))
    NoChange _ -> pure ()

-- ============================================================================
-- Patch Builder
-- ============================================================================

buildResourcePatch :: Resource -> Resource -> [CompetenceLevelId] -> ResourcePatch
buildResourcePatch old new matchedCompetences =
  ResourcePatch
    { identifier = if old.identifier == new.identifier then Nothing else Just (old.identifier, new.identifier)
    , competenceLevels =
        if old.competenceLevels == matchedCompetences
          then Nothing
          else Just (old.competenceLevels, matchedCompetences)
    , content = if old.content == new.content then Nothing else Just (old.content, new.content)
    , attachments = Nothing
    }

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.Planning.ImportModal
-- Description : Modal component for importing lessons
--
-- Provides a modal dialog for importing lessons from a markdown-like
-- format into a specific MesoPlan. Shows a preview of changes before applying.
module Competences.Frontend.Component.Planning.ImportModal
  ( lessonImportModalComponent
  , openLessonImportModal
  , Action
  )
where

import Competences.Command qualified as Cmd
import Competences.Command (EntityCommand (CreateAndLock), LessonPatch (..), ModifyCommand (..))
import Competences.Document (Document (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Lesson (ActionForm (..), Lesson (..))
import Competences.Document.MesoPlan (MesoPlanId)
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
import Competences.Import.LessonParser (parseLessonImport)
import Competences.Import.Matching (matchLessonImport)
import Competences.Import.Types
  ( CompetenceMatch (..)
  , ImportAction (..)
  , LessonImportPreview (..)
  , ParsedLessonPhase (..)
  , actionFormToGerman
  , levelToGerman
  , socialFormToGerman
  )
import Competences.TaskContent.RichContent (toRawText)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((&), (.~))

-- ============================================================================
-- Types (re-exports from generic module)
-- ============================================================================

type Action = IM.Action

-- | Open the lesson import modal as a framed modal.
openLessonImportModal :: SyncContext -> MesoPlanId -> IO ()
openLessonImportModal r mesoPlanId =
  let cfg = ModalConfig (WindowChrome (C.translate' C.LblImportLessons) Icon.IcnImport) (ModalId "import-lessons") ModalWide ModalFull Nothing
   in openFramedModalWith r.windowManager cfg (lessonImportModalComponent r mesoPlanId)

-- ============================================================================
-- Component
-- ============================================================================

lessonImportModalComponent :: SyncContext -> MesoPlanId -> WindowMode -> M.Component p (IM.Model LessonImportPreview) Action
lessonImportModalComponent r mesoPlanId =
  IM.importModalComponent (lessonImportConfig mesoPlanId) r

lessonImportConfig :: MesoPlanId -> IM.ImportModalConfig LessonImportPreview
lessonImportConfig mesoPlanId =
  IM.ImportModalConfig
    { parse = \doc input -> case parseLessonImport input of
        Left err -> Left err
        Right parsed -> Right $ matchLessonImport doc mesoPlanId parsed
    , renderItem = previewLessonView
    , hasChanges = lessonHasChanges
    , apply = applyLessonImport
    , placeholder =
        "# Einführung Gleichungen\n\n\
        \## Angaben\n\
        \Date: 2026-03-15\n\n\
        \## Beschreibung\n\
        \Erste Einheit zum Thema...\n\n\
        \## Kompetenzen\n\
        \- Rastername / Kompetenz / Wesentlich\n\n\
        \## Materialien\n\
        \- Buch S.42\n\n\
        \## Aufgaben\n\
        \- Mathematik-Test 3a\n\n\
        \## Phasen\n\
        \- Einstieg / Plenum / Darbietend / 10 min\n\
        \  Wiederholung der letzten Stunde.\n\n\
        \## Notizen\n\
        \Notizen hier..."
    }

-- ============================================================================
-- Preview View
-- ============================================================================

previewLessonView :: LessonImportPreview -> M.View (IM.Model LessonImportPreview) IM.Action
previewLessonView preview =
  M.div_
    [class_ "border border-border rounded-md p-3"]
    [ -- Lesson header
      M.div_
        [class_ "mb-2"]
        [ Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
            [ M.span_ [class_ "font-semibold"] [M.text $ M.ms $ lessonTitle preview.lessonAction]
            , IM.actionBadge preview.lessonAction
            ]
        ]
    , -- Date info
      case lessonDate preview.lessonAction of
        Just d -> M.div_ [class_ "text-sm text-muted-foreground mb-1"]
          [M.text $ "Datum: " <> C.formatDay d]
        Nothing -> M.text ""
    , -- Description preview
      let desc = lessonDescription preview.lessonAction
       in if T.null desc
            then M.text ""
            else M.div_ [class_ "text-sm text-muted-foreground mb-2"]
              [M.text $ M.ms $ T.take 80 desc <> if T.length desc > 80 then "..." else ""]
    , -- Phases preview
      if null preview.parsedPhases
        then M.text ""
        else M.div_ [class_ "mt-1 mb-2"]
          [ M.div_ [class_ "text-xs font-medium text-muted-foreground mb-1"]
              [M.text $ "Phasen (" <> M.ms (show (length preview.parsedPhases)) <> ")"]
          , M.div_ [class_ "space-y-1"] (map phasePreview preview.parsedPhases)
          ]
    , -- Competence matches
      if null preview.competenceMatches
        then M.text ""
        else
          M.div_
            [class_ "mt-1 space-y-1"]
            (map competenceMatchView preview.competenceMatches)
    ]

lessonTitle :: ImportAction Lesson -> Text
lessonTitle (Create l) = l.title
lessonTitle (Update _ l) = l.title
lessonTitle (NoChange l) = l.title

lessonDate :: ImportAction Lesson -> Maybe Day
lessonDate (Create l) = l.date
lessonDate (Update _ l) = l.date
lessonDate (NoChange l) = l.date

lessonDescription :: ImportAction Lesson -> Text
lessonDescription action =
  let l = case action of
        Create x -> x
        Update _ x -> x
        NoChange x -> x
   in toRawText l.description

phasePreview :: ParsedLessonPhase -> M.View (IM.Model LessonImportPreview) IM.Action
phasePreview phase =
  let borderColor = case phase.actionForm of
        Presenting -> "border-l-red-500"
        Collaborating -> "border-l-orange-500"
        Assigning -> "border-l-green-500"
   in M.div_
        [class_ $ "text-xs p-1.5 bg-muted/30 rounded border-l-2 " <> borderColor]
        [ Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
            [ M.span_ [class_ "font-medium"] [M.text $ M.ms phase.title]
            , M.span_ [class_ "text-muted-foreground"]
                [ M.text $ M.ms (show phase.duration) <> " min"
                , M.text " · "
                , M.text $ M.ms $ socialFormToGerman phase.socialForm
                , M.text " · "
                , M.text $ M.ms $ actionFormToGerman phase.actionForm
                ]
            ]
        ]

competenceMatchView :: CompetenceMatch -> M.View (IM.Model LessonImportPreview) IM.Action
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

lessonHasChanges :: LessonImportPreview -> Bool
lessonHasChanges preview = isChange preview.lessonAction

isChange :: ImportAction a -> Bool
isChange (Create _) = True
isChange (Update _ _) = True
isChange (NoChange _) = False

-- ============================================================================
-- Apply Import
-- ============================================================================

applyLessonImport :: SyncContext -> WindowMode -> Document -> [LessonImportPreview] -> IO ()
applyLessonImport r wm _doc previews = do
  mapM_ (applyLessonPreview r) previews
  closeWindow wm

applyLessonPreview :: SyncContext -> LessonImportPreview -> IO ()
applyLessonPreview r preview = do
  let matchedCompetences = mapMaybe (.matched) preview.competenceMatches

  case preview.lessonAction of
    Create lesson -> do
      newId <- nextId r
      let newLesson = lesson & #id .~ newId & #competenceLevels .~ matchedCompetences
      modifySyncDocument r (Cmd.Lessons $ Cmd.OnLessons $ CreateAndLock newLesson)
    Update old new -> do
      modifySyncDocument r (Cmd.Lessons $ Cmd.OnLessons $ Cmd.Modify old.id Lock)
      let patch = buildLessonPatch old new matchedCompetences
      modifySyncDocument r (Cmd.Lessons $ Cmd.OnLessons $ Cmd.Modify old.id (Release patch))
    NoChange _ -> pure ()

-- ============================================================================
-- Patch Builder
-- ============================================================================

buildLessonPatch :: Lesson -> Lesson -> [CompetenceLevelId] -> LessonPatch
buildLessonPatch old new matchedCompetences =
  LessonPatch
    { title = if old.title == new.title then Nothing else Just (old.title, new.title)
    , description = if old.description == new.description then Nothing else Just (old.description, new.description)
    , competenceLevels =
        if old.competenceLevels == matchedCompetences
          then Nothing
          else Just (old.competenceLevels, matchedCompetences)
    , date = if old.date == new.date then Nothing else Just (old.date, new.date)
    , assignments = if old.assignments == new.assignments then Nothing else Just (old.assignments, new.assignments)
    , resources = if old.resources == new.resources then Nothing else Just (old.resources, new.resources)
    , phases = if old.phases == new.phases then Nothing else Just (old.phases, new.phases)
    , notes = if old.notes == new.notes then Nothing else Just (old.notes, new.notes)
    }

module Competences.Frontend.Component.Resource.EditorDetail
  ( editorDetailView
  , resourceInlineEditor
  )
where

import Competences.Command (Command (..), ResourcesCommand (..))
import Competences.Command.Common qualified as EC
import Competences.Command.Resources (ResourcePatch (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , Lock (..)
  , Resource (..)
  , ResourceContent (..)
  , ResourceIdentifier (..)
  )
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.EditorField (EditorField (..))
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Editor.Types (Action (..), Model (..))
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..))
import Competences.Frontend.Component.Selector.CompetenceLevelSelector (competenceLevelEditorField)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.View.TaskContent (renderRichText)
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind (class_)
import Data.Map.Strict qualified as Map
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as M
import Optics.Core ((&), (?~), (^.))

-- | Detail view for editing a resource
editorDetailView
  :: SyncContext
  -> Resource
  -> M.View (SD.Model Resource mode) (SD.Action mode)
editorDetailView r resource =
  V.component
    ("resource-editor-" <> M.ms (show resource.id))
    (TE.editorComponent resourceEditor r)
  where
    resourceEditable =
      TE.editable
        ( \d ->
            fmap
              (\res -> (res, (d ^. #locks) Map.!? ResourceLock res.id))
              (Ix.getOne $ d.resources Ix.@= resource.id)
        )
        & (#modify ?~ (\res modify -> Resources $ OnResources (EC.Modify res.id modify)))
        & (#delete ?~ (\res -> Resources $ OnResources (EC.Delete res.id)))

    resourceEditor =
      TE.editor
        ( TE.editorFormView'
            (C.translate' C.LblEditResource)
            id
        )
        resourceEditable
        `TE.addNamedField` ( C.translate' C.LblResourceIdentifier
                           , identifierEditorField
                           )
        `TE.addNamedField` ( C.translate' C.LblResourceCompetenceLevels
                           , competenceLevelEditorField r "resource-levels" 1 competenceLevelsLens  -- minResults=1: resources must have at least one level
                           )
        `TE.addNamedField` ( C.translate' C.LblResourceContent
                           , resourceContentEditorField
                           )

-- | Inline editor for a resource - simpler version without the full header
-- This is used in the modal where we just need the edit fields
resourceInlineEditor
  :: SyncContext
  -> Resource
  -> M.View p a
resourceInlineEditor r resource =
  V.component
    ("resource-inline-editor-" <> M.ms (show resource.id))
    (TE.editorComponent inlineEditor r)
  where
    resourceEditable =
      TE.editable
        ( \d ->
            fmap
              (\res -> (res, (d ^. #locks) Map.!? ResourceLock res.id))
              (Ix.getOne $ d.resources Ix.@= resource.id)
        )
        & (#modify ?~ (\res modify -> Resources $ OnResources (EC.Modify res.id modify)))

    -- Inline editor without header or delete button
    inlineEditor =
      TE.editor
        (TE.editorFormViewInline id)  -- Use inline form view (no header)
        resourceEditable
        `TE.addNamedField` ( C.translate' C.LblResourceIdentifier
                           , identifierEditorField
                           )
        `TE.addNamedField` ( C.translate' C.LblResourceCompetenceLevels
                           , competenceLevelEditorField r "resource-inline-levels" 1 competenceLevelsLens  -- minResults=1: resources must have at least one level
                           )
        `TE.addNamedField` ( C.translate' C.LblResourceContent
                           , resourceContentEditorField
                           )

-- | Editor field for identifier (handles ResourceIdentifier newtype)
identifierEditorField :: EditorField Resource ResourcePatch f
identifierEditorField =
  EditorField
    { viewer = \res ->
        let ResourceIdentifier t = res.identifier
         in V.text_ (M.ms t)
    , editor = \refocusTarget original patch ->
        let ResourceIdentifier origText = original.identifier
            currentText = case patch.identifier of
              Just (_, ResourceIdentifier t) -> t
              Nothing -> origText
         in MH.input_ $
              [ V.fullWidth
              , MH.onChange
                  (\v -> UpdatePatch original (patch & #identifier ?~ (original.identifier, ResourceIdentifier (M.fromMisoString v))))
              , M.value_ (M.ms currentText)
              ]
              <> if refocusTarget then [M.id_ "refocus-target"] else []
    }

-- | Lens for competence levels
competenceLevelsLens :: EntityPatchTransformedLens Resource ResourcePatch [] CompetenceLevelId [] CompetenceLevelId
competenceLevelsLens =
  EntityPatchTransformedLens
    { viewLens = #competenceLevels
    , patchLens = #competenceLevels
    , transform = id
    , embed = id
    }

-- | Editor field for resource content (sum type)
-- For now, supports InlineContent only - will be enhanced later for WebLink and VideoLink
resourceContentEditorField :: EditorField Resource ResourcePatch f
resourceContentEditorField =
  EditorField
    { viewer = contentViewer
    , editor = contentEditor
    }
  where
    contentViewer :: Resource -> M.View (Model Resource ResourcePatch f) (Action Resource ResourcePatch)
    contentViewer res = case res.content of
      InlineContent text ->
        if text == ""
          then MH.span_ [class_ "text-stone-400 italic"] [M.text "Kein Inhalt"]
          else MH.div_ [class_ "prose prose-stone prose-sm max-w-none"]
                 [renderRichText text]
      WebLink url desc ->
        MH.div_ [class_ "space-y-1"]
          [ MH.div_ [class_ "flex items-center gap-2"]
              [ MH.span_ [class_ "text-xs font-medium text-stone-500"] [M.text "Web-Link"]
              , MH.a_ [M.href_ (M.ms url), M.target_ "_blank", class_ "text-sky-600 hover:underline"]
                  [M.text $ M.ms url]
              ]
          , if desc /= ""
              then MH.p_ [class_ "text-sm text-stone-600"] [M.text $ M.ms desc]
              else V.empty
          ]
      VideoLink url desc ->
        MH.div_ [class_ "space-y-1"]
          [ MH.div_ [class_ "flex items-center gap-2"]
              [ MH.span_ [class_ "text-xs font-medium text-stone-500"] [M.text "Video-Link"]
              , MH.a_ [M.href_ (M.ms url), M.target_ "_blank", class_ "text-sky-600 hover:underline"]
                  [M.text $ M.ms url]
              ]
          , if desc /= ""
              then MH.p_ [class_ "text-sm text-stone-600"] [M.text $ M.ms desc]
              else V.empty
          ]

    contentEditor
      :: Bool
      -> Resource
      -> ResourcePatch
      -> M.View (Model Resource ResourcePatch f) (Action Resource ResourcePatch)
    contentEditor refocusTarget original patch =
      let currentContent = case patch.content of
            Just (_, c) -> c
            Nothing -> original.content
       in MH.div_ [class_ "space-y-3"]
            [ -- Content type selector
              MH.div_ [class_ "flex gap-2"]
                [ contentTypeButton "Inline" (isInline currentContent) (switchToInline original patch)
                , contentTypeButton "Web-Link" (isWebLink currentContent) (switchToWebLink original patch)
                , contentTypeButton "Video" (isVideoLink currentContent) (switchToVideoLink original patch)
                ]
            , -- Content-specific fields
              case currentContent of
                InlineContent text ->
                  MH.div_ [class_ "flex gap-4 w-full"]
                    [ -- Editor panel (left)
                      MH.div_ [class_ "flex-1 min-w-0"]
                        [ MH.span_ [class_ "block text-sm font-medium text-stone-600 mb-1"] [M.text "Markup"]
                        , MH.textarea_
                            ( [ class_ "w-full min-h-[150px] resize-y font-mono text-sm p-2 border border-stone-300 rounded-md"
                              , MH.onChange
                                  (\v -> UpdatePatch original (patch & #content ?~ (original.content, InlineContent (M.fromMisoString v))))
                              , M.value_ (M.ms text)
                              ]
                              <> if refocusTarget then [M.id_ "refocus-target"] else []
                            )
                            []
                        ]
                    , -- Preview panel (right)
                      MH.div_ [class_ "flex-1 min-w-0"]
                        [ MH.span_ [class_ "block text-sm font-medium text-stone-600 mb-1"] [M.text "Preview"]
                        , MH.div_ [class_ "min-h-[150px] p-3 border border-stone-200 rounded-md bg-stone-50 overflow-auto"]
                            [ if text == ""
                                then MH.span_ [class_ "text-stone-400 italic"] [M.text "Kein Inhalt"]
                                else renderRichText text
                            ]
                        ]
                    ]
                WebLink url desc ->
                  MH.div_ [class_ "space-y-2"]
                    [ MH.div_ []
                        [ MH.span_ [class_ "block text-sm font-medium text-stone-600 mb-1"] [M.text $ C.translate' C.LblUrl]
                        , MH.input_
                            [ class_ "w-full p-2 border border-stone-300 rounded-md"
                            , M.type_ "url"
                            , M.placeholder_ "https://..."
                            , MH.onChange
                                (\v -> UpdatePatch original (patch & #content ?~ (original.content, WebLink (M.fromMisoString v) desc)))
                            , M.value_ (M.ms url)
                            ]
                        ]
                    , MH.div_ []
                        [ MH.span_ [class_ "block text-sm font-medium text-stone-600 mb-1"] [M.text $ C.translate' C.LblDescription]
                        , MH.textarea_
                            [ class_ "w-full min-h-[80px] resize-y p-2 border border-stone-300 rounded-md"
                            , M.placeholder_ "Beschreibung des Links..."
                            , MH.onChange
                                (\v -> UpdatePatch original (patch & #content ?~ (original.content, WebLink url (M.fromMisoString v))))
                            , M.value_ (M.ms desc)
                            ]
                            []
                        ]
                    ]
                VideoLink url desc ->
                  MH.div_ [class_ "space-y-2"]
                    [ MH.div_ []
                        [ MH.span_ [class_ "block text-sm font-medium text-stone-600 mb-1"] [M.text $ C.translate' C.LblUrl]
                        , MH.input_
                            [ class_ "w-full p-2 border border-stone-300 rounded-md"
                            , M.type_ "url"
                            , M.placeholder_ "https://youtube.com/..."
                            , MH.onChange
                                (\v -> UpdatePatch original (patch & #content ?~ (original.content, VideoLink (M.fromMisoString v) desc)))
                            , M.value_ (M.ms url)
                            ]
                        ]
                    , MH.div_ []
                        [ MH.span_ [class_ "block text-sm font-medium text-stone-600 mb-1"] [M.text $ C.translate' C.LblDescription]
                        , MH.textarea_
                            [ class_ "w-full min-h-[80px] resize-y p-2 border border-stone-300 rounded-md"
                            , M.placeholder_ "Beschreibung des Videos..."
                            , MH.onChange
                                (\v -> UpdatePatch original (patch & #content ?~ (original.content, VideoLink url (M.fromMisoString v))))
                            , M.value_ (M.ms desc)
                            ]
                            []
                        ]
                    ]
            ]

    isInline (InlineContent _) = True
    isInline _ = False

    isWebLink (WebLink _ _) = True
    isWebLink _ = False

    isVideoLink (VideoLink _ _) = True
    isVideoLink _ = False

    switchToInline original patch = UpdatePatch original (patch & #content ?~ (original.content, InlineContent ""))
    switchToWebLink original patch = UpdatePatch original (patch & #content ?~ (original.content, WebLink "" ""))
    switchToVideoLink original patch = UpdatePatch original (patch & #content ?~ (original.content, VideoLink "" ""))

    contentTypeButton label isActive action =
      MH.button_
        [ class_ $ "px-3 py-1.5 text-sm rounded-md transition-colors "
            <> if isActive
                 then "bg-sky-600 text-white"
                 else "bg-stone-100 text-stone-700 hover:bg-stone-200"
        , MH.onClick action
        ]
        [M.text label]

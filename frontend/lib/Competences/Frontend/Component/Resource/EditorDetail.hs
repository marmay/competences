module Competences.Frontend.Component.Resource.EditorDetail
  ( editorDetailView
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
import Competences.Frontend.Component.MarkdownEditor (richContentEditorComponent)
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.TaskContent.RichContent (RichContent)
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.Component.Editor.EditorField (mkFieldLens)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Component (component, componentA)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Text (text_)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map.Strict qualified as Map
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as M
import Optics.Core (Lens', lens, (&), (.~), (?~), (^.))

-- | Detail view for editing a resource
editorDetailView
  :: SyncContext
  -> Resource
  -> M.View p a
editorDetailView r resource =
  component
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

-- | Editor field for identifier (handles ResourceIdentifier newtype)
identifierEditorField :: EditorField Resource ResourcePatch f
identifierEditorField =
  EditorField
    { viewer = \res ->
        let ResourceIdentifier t = res.identifier
         in text_ (M.ms t)
    , editor = \refocusTarget original patch ->
        let ResourceIdentifier origText = original.identifier
            currentText = case patch.identifier of
              Just (_, ResourceIdentifier t) -> t
              Nothing -> origText
         in MH.input_ $
              [ class_ "w-full"
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
      InlineContent rc ->
        if rc == mempty
          then Typography.placeholder "Kein Inhalt"
          else MH.div_ [class_ "prose prose-stone prose-sm max-w-none"]
                 [renderRichText rc]
      WebLink url desc ->
        MH.div_ [class_ "space-y-1"]
          [ Layout.hFlow
              (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
              [ MH.span_ [] [Typography.fieldLabel "Web-Link"]
              , MH.a_ [M.href_ (M.ms url), M.target_ "_blank", class_ "text-sky-600 hover:underline"]
                  [M.text $ M.ms url]
              ]
          , if desc /= ""
              then MH.p_ [class_ "text-sm text-muted-foreground"] [M.text $ M.ms desc]
              else Layout.empty
          ]
      VideoLink url desc ->
        MH.div_ [class_ "space-y-1"]
          [ Layout.hFlow
              (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
              [ MH.span_ [] [Typography.fieldLabel "Video-Link"]
              , MH.a_ [M.href_ (M.ms url), M.target_ "_blank", class_ "text-sky-600 hover:underline"]
                  [M.text $ M.ms url]
              ]
          , if desc /= ""
              then MH.p_ [class_ "text-sm text-muted-foreground"] [M.text $ M.ms desc]
              else Layout.empty
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
              Button.buttonGroup
                [ Button.toggleSm (isInline currentContent) (Button.button ("Inline" :: M.MisoString) (switchToInline original patch))
                , Button.toggleSm (isWebLink currentContent) (Button.button ("Web-Link" :: M.MisoString) (switchToWebLink original patch))
                , Button.toggleSm (isVideoLink currentContent) (Button.button ("Video" :: M.MisoString) (switchToVideoLink original patch))
                ]
            , -- Content-specific fields
              case currentContent of
                InlineContent rc ->
                  componentA "rc-resource-editor"
                    (if refocusTarget then [M.id_ "refocus-target"] else [])
                    (richContentEditorComponent rc (resourceRichContentLens original))
                WebLink url desc ->
                  MH.div_ [class_ "space-y-2"]
                    [ MH.div_ []
                        [ MH.span_ [class_ "block mb-1"] [Typography.fieldLabel $ C.translate' C.LblUrl]
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
                        [ MH.span_ [class_ "block mb-1"] [Typography.fieldLabel $ C.translate' C.LblDescription]
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
                        [ MH.span_ [class_ "block mb-1"] [Typography.fieldLabel $ C.translate' C.LblUrl]
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
                        [ MH.span_ [class_ "block mb-1"] [Typography.fieldLabel $ C.translate' C.LblDescription]
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

    switchToInline original patch = UpdatePatch original (patch & #content ?~ (original.content, InlineContent mempty))
    switchToWebLink original patch = UpdatePatch original (patch & #content ?~ (original.content, WebLink "" ""))
    switchToVideoLink original patch = UpdatePatch original (patch & #content ?~ (original.content, VideoLink "" ""))

-- | Lens into the 'RichContent' inside a resource's 'InlineContent'.
--
-- GET: extracts 'RichContent' from 'InlineContent', returns 'mempty' for other variants.
-- SET: wraps the 'RichContent' in 'InlineContent' and updates the patch.
--
-- This is safe because the component is only rendered when the content type is InlineContent.
resourceRichContentLens :: Resource -> Lens' (Model Resource ResourcePatch f) RichContent
resourceRichContentLens original = lens getter setter
  where
    baseLens = mkFieldLens #content #content original
    getter model = case model ^. baseLens of
      InlineContent rc -> rc
      _ -> mempty
    setter model rc =
      model & baseLens .~ InlineContent rc

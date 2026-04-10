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
  , FileRef (..)
  , Lock (..)
  , LockHolder (..)
  , Resource (..)
  , ResourceContent (..)
  , ResourceIdentifier (..)
  , SHA256Hash (..)
  )
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.EditorField (EditorField (..), mkFieldLens)
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Editor.Types (Action (..), Model (..))
import Competences.Frontend.Component.FileUpload (fileUploadComponent, showFileSize)
import Competences.Frontend.Component.MarkdownEditor (ContentState (..), richContentEditorComponent)
import Competences.Frontend.Component.RichContent (renderRichTextWithFiles)
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..))
import Competences.Frontend.Component.Selector.CompetenceLevelSelector (competenceLevelEditorField)
import Competences.Frontend.SyncContext (SyncContext (..))
import Competences.Frontend.SyncContext.WindowManager (inlineComponent, inlineComponentAttrs)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Text (text_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.TaskContent.RichContent (RichContent)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as M
import Miso.String (ms)
import Optics.Core (Lens', lens, (&), (.~), (?~), (^.))

-- | Detail view for editing a resource
editorDetailView
  :: SyncContext
  -> Resource
  -> M.View p a
editorDetailView r resource =
  inlineComponent
    ("resource-editor-" <> M.ms (show resource.id))
    (TE.editorComponent resourceEditor r)
  where
    resourceEditable =
      TE.editable
        ( \d ->
            fmap
              (\res -> (res, fmap (.userId) $ (d ^. #locks) Map.!? ResourceLock res.id))
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
                           , resourceContentEditorField r
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
resourceContentEditorField :: SyncContext -> EditorField Resource ResourcePatch f
resourceContentEditorField r =
  EditorField
    { viewer = contentViewer
    , editor = contentEditor
    }
  where
    fc = r.formulaCache

    contentViewer :: Resource -> M.View (Model Resource ResourcePatch f) (Action Resource ResourcePatch)
    contentViewer res = case res.content of
      InlineContent rc ->
        if rc == mempty
          then Typography.placeholder "Kein Inhalt"
          else MH.div_ [class_ "prose prose-stone prose-sm max-w-none"]
                 [renderRichTextWithFiles fc r res.attachments rc]
      WebLink url desc ->
        MH.div_ [class_ "space-y-1"]
          [ Layout.hFlow
              (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
              [ MH.span_ [] [Typography.fieldLabel $ C.translate' C.LblWebLink]
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
              [ MH.span_ [] [Typography.fieldLabel $ C.translate' C.LblVideoLink]
              , MH.a_ [M.href_ (M.ms url), M.target_ "_blank", class_ "text-sky-600 hover:underline"]
                  [M.text $ M.ms url]
              ]
          , if desc /= ""
              then MH.p_ [class_ "text-sm text-muted-foreground"] [M.text $ M.ms desc]
              else Layout.empty
          ]
      FileContent fileRef ->
        MH.div_ [class_ "space-y-1"]
          [ Layout.hFlow
              (Layout.gapS <> Layout.crossCenter)
              [ Typography.fieldLabel $ C.translate' C.LblFile
              , M.text $ ms fileRef.fileName
              ]
          , Typography.small $ ms $
              fileRef.mimeType <> " (" <> showFileSize fileRef.fileSize <> ")"
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
                [ Button.toggleSm (isInline currentContent) (Button.button (C.translate' C.LblInlineContent) (switchToInline original patch))
                , Button.toggleSm (isWebLink currentContent) (Button.button (C.translate' C.LblWebLink) (switchToWebLink original patch))
                , Button.toggleSm (isVideoLink currentContent) (Button.button (C.translate' C.LblVideoLink) (switchToVideoLink original patch))
                , Button.toggleSm (isFile currentContent) (Button.button (C.translate' C.LblFile) (switchToFile original patch))
                ]
            , -- Content-specific fields
              case currentContent of
                InlineContent rc ->
                  MH.div_ [class_ "space-y-3"]
                    [ inlineComponentAttrs "rc-resource-editor"
                        (if refocusTarget then [M.id_ "refocus-target"] else [])
                        (richContentEditorComponent fc rc (resourceRichContentLens original))
                    , inlineComponent "resource-attachments-upload"
                        (fileUploadComponent r
                          (Just $ C.translate' C.LblAttachments)
                          (currentAttachments original patch)
                          (resourceAttachmentsLens original))
                    ]
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
                FileContent fileRef ->
                  inlineComponent "file-upload-editor"
                    (fileUploadComponent r
                      Nothing
                      (if isNilFileRef fileRef then [] else [fileRef])
                      (resourceFileRefsLens original))
            ]

    isInline (InlineContent _) = True
    isInline _ = False

    isWebLink (WebLink _ _) = True
    isWebLink _ = False

    isVideoLink (VideoLink _ _) = True
    isVideoLink _ = False

    isFile (FileContent _) = True
    isFile _ = False

    switchToInline original patch = UpdatePatch original (patch & #content ?~ (original.content, InlineContent mempty))
    switchToWebLink original patch = UpdatePatch original (patch & #content ?~ (original.content, WebLink "" ""))
    switchToVideoLink original patch = UpdatePatch original (patch & #content ?~ (original.content, VideoLink "" ""))
    switchToFile original patch = UpdatePatch original (patch & #content ?~ (original.content, FileContent nilFileRef))

-- | Get the current attachments value, considering any pending patch.
currentAttachments :: Resource -> ResourcePatch -> [FileRef]
currentAttachments original patch = case patch.attachments of
  Just (_, after) -> after
  Nothing -> original.attachments

-- | Lens into the 'attachments' field of a resource via its patch.
resourceAttachmentsLens :: Resource -> Lens' (Model Resource ResourcePatch f) [FileRef]
resourceAttachmentsLens = mkFieldLens #attachments #attachments

-- | A nil FileRef used as placeholder when switching to FileContent mode.
nilFileRef :: FileRef
nilFileRef = FileRef (SHA256Hash "") "" "" 0

-- | Check if a FileRef is the nil placeholder.
isNilFileRef :: FileRef -> Bool
isNilFileRef fr = fr.hash == SHA256Hash ""

-- | Lens into the 'RichContent' inside a resource's 'InlineContent'.
--
-- GET: extracts 'RichContent' from 'InlineContent', returns 'mempty' for other variants.
-- SET: wraps the 'RichContent' in 'InlineContent' and updates the patch.
--
-- This is safe because the component is only rendered when the content type is InlineContent.
resourceRichContentLens :: Resource -> Lens' (Model Resource ResourcePatch f) (ContentState RichContent)
resourceRichContentLens original = lens getter setter
  where
    fieldName = "content" :: Text
    baseLens = mkFieldLens #content #content original
    getter model = case Map.lookup original model.contentStates >>= Map.lookup fieldName of
      Just cs -> cs
      _ -> case model ^. baseLens of
        InlineContent rc -> Valid rc
        _ -> Valid mempty
    setter model cs@(Valid rc) =
      (model{contentStates = insertCS model cs}) & baseLens .~ InlineContent rc
    setter model cs = model{contentStates = insertCS model cs}

    insertCS m cs = Map.alter (Just . Map.insert fieldName cs . fromMaybe Map.empty) original m.contentStates

-- | Lens that maps between @[FileRef]@ (component's model) and 'ResourceContent' (patch).
--
-- GET: extracts @[FileRef]@ from 'FileContent' (singleton or empty).
-- SET: takes @listToMaybe@ of the @[FileRef]@, wraps in 'FileContent', updates patch.
resourceFileRefsLens :: Resource -> Lens' (Model Resource ResourcePatch f) [FileRef]
resourceFileRefsLens original = lens getter setter
  where
    baseLens = mkFieldLens #content #content original
    getter model = case model ^. baseLens of
      FileContent ref
        | isNilFileRef ref -> []
        | otherwise -> [ref]
      _ -> []
    setter model refs =
      case listToMaybe refs of
        Just ref -> model & baseLens .~ FileContent ref
        Nothing -> model & baseLens .~ FileContent nilFileRef

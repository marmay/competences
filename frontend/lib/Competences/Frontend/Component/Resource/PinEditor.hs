-- | Resource editor mounted in a pinned dialog.
module Competences.Frontend.Component.Resource.PinEditor
  ( resourcePinEditor
  )
where

import Competences.Command (Command (..), EntityCommand (..), ResourcesCommand (..))
import Competences.Command.Resources (ResourcePatch (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , FileRef (..)
  , Lock (..)
  , Resource (..)
  , ResourceContent (..)
  , ResourceIdentifier (..)
  , SHA256Hash (..)
  , lockOwner
  )
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Resource (ResourceId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor (Editable (..), editable, editor, addNamedField, editorComponent)
import Competences.Frontend.Component.Editor.EditorField (EditorField (..), mkFieldLens)
import Competences.Frontend.Component.Editor.FormView (editorFormView')
import Competences.Frontend.Component.Editor.Types (Action (..), Model (..), singlePatchLens)
import Competences.Frontend.Component.FileUpload (fileUploadComponent, showFileSize)
import Competences.Frontend.Component.MarkdownEditor (ContentState (..), richContentEditorComponent)
import Competences.Frontend.Component.RichContent (renderRichTextWithFiles)
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..))
import Competences.Frontend.Component.Selector.CompetenceLevelSelector (competenceLevelEditorField)
import Competences.Frontend.SyncContext (SyncContext (..))
import Competences.Frontend.SyncContext.WindowManager
  ( PinId
  , WindowMode
  , inlineComponent
  , inlineComponentAttrs
  , pinSaveStateLens
  )
import Competences.Frontend.SyncContext.WindowManager qualified as WM (Model)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Text (text_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.TaskContent.RichContent (RichContent)
import Data.Default (Default (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (ms)
import Optics.Core (Lens', lens, (&), (.~), (?~), (^.))
import Optics.Core qualified as O

-- | Resource pin editor factory.
resourcePinEditor
  :: SyncContext -> ResourceId -> PinId
  -> WindowMode -> Maybe ResourcePatch
  -> M.Component WM.Model (Model Resource ResourcePatch Maybe) (Action Resource ResourcePatch)
resourcePinEditor r resId pid _mode mSaved =
  (editorComponent resourceEditor r (fromMaybe def mSaved))
    { M.bindings =
        [ O.toLensVL (pinSaveStateLens pid) M.<--- O.toLensVL singlePatchLens
        ]
    }
  where
    resourceEditable :: Editable Maybe Resource ResourcePatch
    resourceEditable =
      editable
        ( \d ->
            fmap
              (\res -> (res, lockOwner (ResourceLock res.id) d))
              (Ix.getOne $ d.resources Ix.@= resId)
        )
        & (#modify ?~ (\res modify -> Resources $ OnResources (Modify res.id modify)))

    resourceEditor =
      editor
        (editorFormView' (C.translate' C.LblEditResource) id)
        resourceEditable
        `addNamedField` ( C.translate' C.LblResourceIdentifier
                        , identifierEditorField
                        )
        `addNamedField` ( C.translate' C.LblResourceCompetenceLevels
                        , competenceLevelEditorField r "resource-pin-levels" 1 competenceLevelsLens
                        )
        `addNamedField` ( C.translate' C.LblResourceContent
                        , resourceContentEditorField r
                        )

-- ============================================================================
-- Fields (migrated from former Resource/EditorDetail.hs)
-- ============================================================================

-- | Editor field for the resource identifier (ResourceIdentifier newtype wrap).
identifierEditorField :: EditorField Resource ResourcePatch f
identifierEditorField =
  EditorField
    { viewer = \res ->
        let ResourceIdentifier t = res.identifier
         in text_ (ms t)
    , editor = \refocusTarget original patch ->
        let ResourceIdentifier origText = original.identifier
            currentText = case patch.identifier of
              Just (_, ResourceIdentifier t) -> t
              Nothing -> origText
         in MH.input_ $
              [ class_ "w-full"
              , MH.onChange
                  (\v -> UpdatePatch original (patch & #identifier ?~ (original.identifier, ResourceIdentifier (M.fromMisoString v))))
              , MP.value_ (ms currentText)
              ]
              <> if refocusTarget then [MP.id_ "refocus-target"] else []
    }

-- | Lens for competence levels; resources must have at least one (enforced by the selector's @minResults = 1@).
competenceLevelsLens :: EntityPatchTransformedLens Resource ResourcePatch [] CompetenceLevelId [] CompetenceLevelId
competenceLevelsLens =
  EntityPatchTransformedLens
    { viewLens = #competenceLevels
    , patchLens = #competenceLevels
    , transform = id
    , embed = id
    }

-- | Editor field for the resource content sum type.
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
          else
            MH.div_
              [class_ "prose prose-stone prose-sm max-w-none"]
              [renderRichTextWithFiles fc r res.attachments rc]
      WebLink url desc ->
        MH.div_
          [class_ "space-y-1"]
          [ Layout.hFlow
              (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
              [ MH.span_ [] [Typography.fieldLabel $ C.translate' C.LblWebLink]
              , MH.a_ [MP.href_ (ms url), MP.target_ "_blank", class_ "text-sky-600 hover:underline"]
                  [M.text $ ms url]
              ]
          , if desc /= ""
              then MH.p_ [class_ "text-sm text-muted-foreground"] [M.text $ ms desc]
              else Layout.empty
          ]
      VideoLink url desc ->
        MH.div_
          [class_ "space-y-1"]
          [ Layout.hFlow
              (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
              [ MH.span_ [] [Typography.fieldLabel $ C.translate' C.LblVideoLink]
              , MH.a_ [MP.href_ (ms url), MP.target_ "_blank", class_ "text-sky-600 hover:underline"]
                  [M.text $ ms url]
              ]
          , if desc /= ""
              then MH.p_ [class_ "text-sm text-muted-foreground"] [M.text $ ms desc]
              else Layout.empty
          ]
      FileContent fileRef ->
        MH.div_
          [class_ "space-y-1"]
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
            [ Button.buttonGroup
                [ Button.toggleSm (isInline currentContent) (Button.button (C.translate' C.LblInlineContent) (switchToInline original patch))
                , Button.toggleSm (isWebLink currentContent) (Button.button (C.translate' C.LblWebLink) (switchToWebLink original patch))
                , Button.toggleSm (isVideoLink currentContent) (Button.button (C.translate' C.LblVideoLink) (switchToVideoLink original patch))
                , Button.toggleSm (isFile currentContent) (Button.button (C.translate' C.LblFile) (switchToFile original patch))
                ]
            , case currentContent of
                InlineContent rc ->
                  MH.div_
                    [class_ "space-y-3"]
                    [ inlineComponentAttrs
                        "rc-resource-editor"
                        (if refocusTarget then [MP.id_ "refocus-target"] else [])
                        (richContentEditorComponent fc rc (resourceRichContentLens original))
                    , inlineComponent
                        "resource-attachments-upload"
                        ( fileUploadComponent
                            r
                            (Just $ C.translate' C.LblAttachments)
                            (currentAttachments original patch)
                            (resourceAttachmentsLens original)
                        )
                    ]
                WebLink url desc ->
                  urlDescForm original patch WebLink url desc "https://..." "Beschreibung des Links..."
                VideoLink url desc ->
                  urlDescForm original patch VideoLink url desc "https://youtube.com/..." "Beschreibung des Videos..."
                FileContent fileRef ->
                  inlineComponent
                    "file-upload-editor"
                    ( fileUploadComponent
                        r
                        Nothing
                        (if isNilFileRef fileRef then [] else [fileRef])
                        (resourceFileRefsLens original)
                    )
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

-- | Shared url + description form for 'WebLink' and 'VideoLink' content variants.
urlDescForm
  :: Resource
  -> ResourcePatch
  -> (Text -> Text -> ResourceContent)
  -- ^ Content constructor (WebLink or VideoLink)
  -> Text
  -- ^ Current URL value
  -> Text
  -- ^ Current description value
  -> M.MisoString
  -- ^ URL placeholder
  -> M.MisoString
  -- ^ Description placeholder
  -> M.View (Model Resource ResourcePatch f) (Action Resource ResourcePatch)
urlDescForm original patch mkContent url desc urlPlaceholder descPlaceholder =
  MH.div_
    [class_ "space-y-2"]
    [ MH.div_ []
        [ MH.span_ [class_ "block mb-1"] [Typography.fieldLabel $ C.translate' C.LblUrl]
        , MH.input_
            [ class_ "w-full p-2 border border-stone-300 rounded-md"
            , MP.type_ "url"
            , MP.placeholder_ urlPlaceholder
            , MH.onChange
                (\v -> UpdatePatch original (patch & #content ?~ (original.content, mkContent (M.fromMisoString v) desc)))
            , MP.value_ (ms url)
            ]
        ]
    , MH.div_ []
        [ MH.span_ [class_ "block mb-1"] [Typography.fieldLabel $ C.translate' C.LblDescription]
        , MH.textarea_
            [ class_ "w-full min-h-[80px] resize-y p-2 border border-stone-300 rounded-md"
            , MP.placeholder_ descPlaceholder
            , MH.onChange
                (\v -> UpdatePatch original (patch & #content ?~ (original.content, mkContent url (M.fromMisoString v))))
            , MP.value_ (ms desc)
            ]
            []
        ]
    ]

currentAttachments :: Resource -> ResourcePatch -> [FileRef]
currentAttachments original patch = case patch.attachments of
  Just (_, after) -> after
  Nothing -> original.attachments

resourceAttachmentsLens :: Resource -> Lens' (Model Resource ResourcePatch f) [FileRef]
resourceAttachmentsLens = mkFieldLens #attachments #attachments

nilFileRef :: FileRef
nilFileRef = FileRef (SHA256Hash "") "" "" 0

isNilFileRef :: FileRef -> Bool
isNilFileRef fr = fr.hash == SHA256Hash ""

-- | Lens into the 'RichContent' inside a resource's 'InlineContent'.
-- Safe because the component is only mounted when the content type is 'InlineContent'.
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
      (model {contentStates = insertCS model cs}) & baseLens .~ InlineContent rc
    setter model cs = model {contentStates = insertCS model cs}

    insertCS m cs = Map.alter (Just . Map.insert fieldName cs . fromMaybe Map.empty) original m.contentStates

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


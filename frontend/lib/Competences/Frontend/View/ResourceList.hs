-- | Shared view for rendering a list of resources with rich display.
--
-- Shows inline content as expandable cards, web/video links as clickable cards.
-- Used by both the Resource Modal and the Lesson Planning DetailView.
module Competences.Frontend.View.ResourceList
  ( resourcesListView
  )
where

import Competences.Document (Resource (..), ResourceContent (..), ResourceIdentifier (..))
import Competences.Document.Resource (ResourceId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.TaskContent.RichContent (toRawText)
import Data.Set qualified as Set
import Data.Text qualified as T
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP

-- | Render a list of resources with rich display.
--
-- Inline content resources are shown as expandable cards (using Disclosure).
-- Web and video links are shown as clickable link cards.
resourcesListView
  :: [Resource]
  -> Set.Set ResourceId
  -> (ResourceId -> action)
  -> M.View model action
resourcesListView resources expandedSet toggleExpanded =
  if null resources
    then Typography.muted $ C.translate' C.LblNoResources
    else MH.div_ [class_ "space-y-2"] (map resourceCard resources)
  where
    resourceCard res =
      let ResourceIdentifier ident = res.identifier
          displayName = if T.null ident then "(Unbenannt)" else ident
          disclosureTitle = Disclosure.titleIconText Icon.IcnResources (M.ms displayName)
          nameView =
            MH.div_
              [class_ "flex items-center gap-2"]
              [ Icon.icon [class_ "text-sky-600"] Icon.IcnResources
              , MH.span_ [class_ "font-medium"] [M.text (M.ms displayName)]
              ]
       in case res.content of
            -- Inline content: expandable card
            InlineContent rc ->
              let isExpanded = Set.member res.id expandedSet
                  hasContent = rc /= mempty
                  bodyView =
                    MH.div_
                      [class_ "prose prose-stone prose-sm max-w-none whitespace-pre-wrap"]
                      [M.text (M.ms (toRawText rc))]
               in if hasContent
                    then
                      Disclosure.disclosure (toggleExpanded res.id) $
                        Disclosure.contents disclosureTitle isExpanded bodyView []
                    else
                      MH.div_
                        [class_ "border rounded-lg overflow-hidden"]
                        [MH.div_ [class_ "flex items-center gap-2 px-3 py-2"] [nameView]]
            -- Web link: direct link card
            WebLink url title ->
              MH.a_
                [ class_ "flex items-center gap-2 px-4 py-3 border rounded-lg hover:bg-muted/50 transition-colors"
                , MP.href_ (M.ms url)
                , MP.target_ "_blank"
                , MP.rel_ "noopener noreferrer"
                ]
                [ Icon.icon [class_ "text-sky-600"] Icon.IcnLink
                , MH.span_ [class_ "font-medium"] [M.text (M.ms displayName)]
                , if T.null title || title == ident
                    then V.empty
                    else MH.span_ [class_ "text-muted-foreground text-sm truncate"] [M.text (M.ms $ "— " <> title)]
                ]
            -- Video link: direct link card
            VideoLink url title ->
              MH.a_
                [ class_ "flex items-center gap-2 px-4 py-3 border rounded-lg hover:bg-muted/50 transition-colors"
                , MP.href_ (M.ms url)
                , MP.target_ "_blank"
                , MP.rel_ "noopener noreferrer"
                ]
                [ Icon.icon [class_ "text-sky-600"] Icon.IcnVideo
                , MH.span_ [class_ "font-medium"] [M.text (M.ms displayName)]
                , if T.null title || title == ident
                    then V.empty
                    else MH.span_ [class_ "text-muted-foreground text-sm truncate"] [M.text (M.ms $ "— " <> title)]
                ]

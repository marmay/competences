-- | A reusable custom select dropdown component.
--
-- Unlike HTML @\<select\>@, supports badges, icons, and rich formatting
-- in both the trigger button and the dropdown items.
module Competences.Frontend.Component.Selector.CustomSelect
  ( CustomSelectConfig (..)
  , CustomSelectModel
  , CustomSelectAction
  , customSelectComponent
  )
where

import Competences.Document (Document)
import Competences.Frontend.Component.Selector.Common (SelectorTransformedLens, mkSelectorBinding)
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext
  , isInitialUpdate
  , subscribeDocument
  )
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Optics.Core ((&), (.~), (^.))

-- | Configuration record: how to derive options and render them.
data CustomSelectConfig k a = CustomSelectConfig
  { deriveOptions :: Document -> [a]
  -- ^ Extract options from the document
  , itemKey :: a -> k
  -- ^ Identity key for stable selection across document updates
  , compact :: forall m act. a -> M.View m act
  -- ^ Trigger display (e.g. badge + date)
  , detailed :: forall m act. a -> M.View m act
  -- ^ Dropdown item display (e.g. badge + date + extra info)
  }

-- | Internal model (opaque).
data CustomSelectModel a = CustomSelectModel
  { isOpen :: !Bool
  , selected :: !(Maybe a)
  , options :: ![a]
  }
  deriving (Generic)

-- Manual Eq to avoid requiring Eq on the config functions
instance (Eq a) => Eq (CustomSelectModel a) where
  a == b = a.isOpen == b.isOpen && a.selected == b.selected && a.options == b.options

instance (Show a) => Show (CustomSelectModel a) where
  show csm =
    "CustomSelectModel {isOpen="
      <> show csm.isOpen
      <> ", selected="
      <> show csm.selected
      <> ", options="
      <> show csm.options
      <> "}"

-- | Internal action (opaque).
data CustomSelectAction a
  = SelectItem !a
  | ToggleOpen
  | CloseSelect
  | CSDocUpdated !DocumentChange
  deriving (Eq, Show)

-- | Construct a custom select component.
customSelectComponent
  :: forall p k a f t
   . (Eq a, Eq k)
  => SyncContext
  -> CustomSelectConfig k a
  -> SelectorTransformedLens p Maybe a f t
  -> M.Component p (CustomSelectModel a) (CustomSelectAction a)
customSelectComponent r (CustomSelectConfig {deriveOptions, itemKey, compact, detailed}) binding =
  (M.component initModel update view')
    { M.bindings = [mkSelectorBinding binding #selected]
    , M.subs = [subscribeDocument r CSDocUpdated]
    }
  where
    initModel = CustomSelectModel {isOpen = False, selected = Nothing, options = []}

    update (CSDocUpdated (DocumentChange doc info)) = M.modify $ \csm ->
      let newOptions = deriveOptions doc
          newSelected
            | isInitialUpdate info = case newOptions of
                (x : _) -> Just x
                [] -> Nothing
            | otherwise = do
                sel <- csm ^. #selected
                let k = itemKey sel
                findByKey k newOptions
       in csm
            & (#options .~ newOptions)
            & (#selected .~ newSelected)

    update (SelectItem a) = M.modify $ \csm ->
      csm & (#selected .~ Just a) & (#isOpen .~ False)

    update ToggleOpen = M.modify $ \csm ->
      csm & (#isOpen .~ not csm.isOpen)

    update CloseSelect = M.modify $ \csm ->
      csm & (#isOpen .~ False)

    findByKey k = foldr (\x acc -> if itemKey x == k then Just x else acc) Nothing

    view' csm =
      MH.div_
        [class_ "relative"]
        [ -- Trigger button
          MH.button_
            [ class_ "flex items-center gap-2 rounded-md border border-input bg-background px-3 py-1.5 text-sm cursor-pointer hover:bg-accent/50 transition-colors"
            , MP.type_ "button"
            , MH.onClick ToggleOpen
            ]
            [ case csm.selected of
                Nothing -> MH.span_ [class_ "text-muted-foreground"] [M.text "—"]
                Just sel -> compact sel
            , Icon.iconS Icon.Small Icon.IcnArrowDown
            ]
        , -- Backdrop (closes dropdown on click-outside)
          if csm.isOpen
            then
              MH.div_
                [ class_ "fixed inset-0 z-40"
                , MH.onClick CloseSelect
                ]
                []
            else M.text ""
        , -- Dropdown
          if csm.isOpen
            then
              MH.div_
                [class_ "absolute right-0 top-full mt-1 z-50 min-w-full w-max bg-popover border border-border rounded-lg shadow-lg py-1"]
                (map (viewDropdownItem csm.selected) csm.options)
            else M.text ""
        ]

    viewDropdownItem mSelected a =
      let isSelected = case mSelected of
            Nothing -> False
            Just sel -> itemKey sel == itemKey a
       in MH.div_
            [ class_ $
                "flex items-center gap-2 px-3 py-1.5 text-sm cursor-pointer transition-colors "
                  <> if isSelected then "bg-accent" else "hover:bg-accent/50"
            , MH.onClick (SelectItem a)
            ]
            [detailed a]

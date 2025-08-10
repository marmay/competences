module Competences.Frontend.View.Layout
  ( hBox_
  , vBox_
  , growing_
  , spring_
  , withMargin_
  , Expand (..)
  , Gap (..)
  , Alignment (..)
  )
where

import Competences.Frontend.View.Tailwind qualified as T
import Miso qualified as M

data Direction = Horizontal | Vertical

data Expand = Expand Alignment | NoExpand

data Gap = NoGap | TinyGap | SmallGap | MediumGap | LargeGap

data Alignment = Start | Center | End

hBox_ :: Expand -> Expand -> Gap -> [M.View m a] -> M.View m a
hBox_ = box_ Horizontal

vBox_ :: Expand -> Expand -> Gap -> [M.View m a] -> M.View m a
vBox_ = box_ Vertical

spring_ :: M.View m a
spring_ = M.div_ [T.tailwind [T.FlexGrow]] []

growing_ :: [M.View m a] -> M.View m a
growing_ = M.div_ [T.tailwind [T.FlexGrow]]

withMargin_ :: M.View m a -> M.View m a
withMargin_ = M.div_ [T.tailwind [T.M4]] . pure

box_ :: Direction -> Expand -> Expand -> Gap -> [M.View m a] -> M.View m a
box_ d eD eO g = M.div_ [T.tailwind $ [T.Flex] <> direction <> expandD <> expandO <> gap]
  where
    direction = case d of
      Horizontal -> [T.FlexRow]
      Vertical -> [T.FlexCol]
    expandD = case (d, eD) of
      (Horizontal, Expand a) -> T.WFull : alignD a
      (Vertical, Expand a) -> alignD a
      (_, NoExpand) -> []
    expandO = case (d, eO) of
      (Horizontal, Expand a) -> T.HFull : alignO a
      (Vertical, Expand a) -> T.WFull : alignO a
      (_, NoExpand) -> []
    gap = case g of
      NoGap -> []
      TinyGap -> [T.Gap1]
      SmallGap -> [T.Gap2]
      MediumGap -> [T.Gap4]
      LargeGap -> [T.Gap8]
    alignD a = case a of
      Start -> [T.JustifyStart]
      Center -> [T.JustifyCenter]
      End -> [T.JustifyEnd]
    alignO a = case a of
      Start -> [T.ItemsStart]
      Center -> [T.ItemsLastBaseline]
      End -> [T.ItemsEnd]

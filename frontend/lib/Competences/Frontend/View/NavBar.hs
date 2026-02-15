-- | Navigation bar with category icons, hover dropdowns, and burger menu.
--
-- This module defines the navigation data types and composes View helpers.
-- It uses HoverMenu, Icon, and Layout for all rendering.
module Competences.Frontend.View.NavBar
  ( NavCategory (..)
  , NavEntry (..)
  , teacherCategories
  , studentCategories
  , teacherExtraCategories
  , navCategoryView
  , burgerMenuView
  )
where

import Competences.Frontend.Common.Translate qualified as C
import Data.List (intercalate)
import Competences.Frontend.Page
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.HoverMenu qualified as HoverMenu
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Miso (View)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (MisoString)

-- | A navigation category with an icon, label, description, main page, and sub-entries.
data NavCategory = NavCategory
  { categoryIcon :: !Icon.Icon
  , categoryLabel :: !MisoString
  , categoryDescription :: !MisoString
  , categoryPage :: !Page
  , subEntries :: ![NavEntry]
  }

-- | A single navigation entry within a category.
data NavEntry = NavEntry
  { entryIcon :: !Icon.Icon
  , entryLabel :: !MisoString
  , entryPage :: !Page
  }

-- | Teacher navigation categories (3 main categories with sub-entries).
teacherCategories :: [NavCategory]
teacherCategories =
  [ NavCategory
      { categoryIcon = Icon.IcnAssignment
      , categoryLabel = C.translate' C.LblAssignments
      , categoryDescription = "Bearbeite deine Aufträge und verfolge deinen Lernfortschritt."
      , categoryPage = ManageAssignments
      , subEntries =
          [ NavEntry Icon.IcnTask (C.translate' C.LblSelfContainedTasks) ManageTasks
          , NavEntry Icon.IcnEvidence (C.translate' C.LblEvidences) Evidences
          ]
      }
  , NavCategory
      { categoryIcon = Icon.IcnCompetenceGrid
      , categoryLabel = C.translate' C.LblCompetenceGrid
      , categoryDescription = "Eine Übersicht darüber, was du schon kannst und was du noch etwas üben musst."
      , categoryPage = CompetenceGrid
      , subEntries = []
      }
  , NavCategory
      { categoryIcon = Icon.IcnLessonNotes
      , categoryLabel = C.translate' C.LblLessonNotesEntries
      , categoryDescription = "Notizen und Materialien aus dem Unterricht."
      , categoryPage = ManageLessonNotes
      , subEntries =
          [ NavEntry Icon.IcnResources (C.translate' C.LblResources) ManageResources
          , NavEntry Icon.IcnMesoPlan (C.translate' C.LblMesoPlanning) Planning
          ]
      }
  ]

-- | Student navigation categories (same 3, no sub-entries).
studentCategories :: [NavCategory]
studentCategories =
  [ NavCategory
      { categoryIcon = Icon.IcnAssignment
      , categoryLabel = C.translate' C.LblAssignments
      , categoryDescription = "Bearbeite deine Aufträge und verfolge deinen Lernfortschritt."
      , categoryPage = ViewAssignments
      , subEntries = []
      }
  , NavCategory
      { categoryIcon = Icon.IcnCompetenceGrid
      , categoryLabel = C.translate' C.LblCompetenceGrid
      , categoryDescription = "Eine Übersicht darüber, was du schon kannst und was du noch etwas üben musst."
      , categoryPage = CompetenceGrid
      , subEntries = []
      }
  , NavCategory
      { categoryIcon = Icon.IcnLessonNotes
      , categoryLabel = C.translate' C.LblLessonNotesEntries
      , categoryDescription = "Notizen und Materialien aus dem Unterricht."
      , categoryPage = ManageLessonNotes
      , subEntries = []
      }
  ]

-- | Extra categories shown only in the burger menu (teacher only).
teacherExtraCategories :: [(MisoString, [NavEntry])]
teacherExtraCategories =
  [ ("Statistik", [NavEntry Icon.IcnProgress (C.translate' C.LblStatisticsOverview) StatisticsOverview])
  , ("Benutzer", [NavEntry Icon.IcnView (C.translate' C.LblManageUsers) ManageUsers])
  ]

-- | Check if a page belongs to a category (main page or any sub-entry).
isInCategory :: Page -> NavCategory -> Bool
isInCategory p cat =
  p == cat.categoryPage || any (\e -> p == e.entryPage) cat.subEntries

-- | Render a single category icon with active underline and rich tooltip.
--
-- Click navigates to the main category page. Hover shows a tooltip with
-- heading + description (all users) and clickable sub-entries (teachers only).
navCategoryView :: Bool -> (Page -> a) -> Page -> NavCategory -> View m a
navCategoryView isTeacher navigate currentPage cat =
  let active = isInCategory currentPage cat
   in MH.div_
        [class_ "group/nav relative"]
        [ MH.div_
            [ class_ $
                "pb-0.5 border-b-2 "
                  <> if active then "border-primary-foreground" else "border-transparent"
            ]
            [ Button.primaryLg (Button.ButtonConfig (Button.SizedIcon Icon.XLarge cat.categoryIcon) (Just (navigate cat.categoryPage)))
            ]
        , categoryTooltip isTeacher navigate currentPage cat
        ]

-- | Rich tooltip: heading + description, with optional sub-entries for teachers.
--
-- Teachers: appears immediately, interactive (clickable sub-entries).
-- Students: appears after 300ms delay, non-interactive.
categoryTooltip :: Bool -> (Page -> a) -> Page -> NavCategory -> View m a
categoryTooltip isTeacher navigate currentPage cat =
  MH.div_
    [ class_ $
        "absolute left-1/2 -translate-x-1/2 top-full pt-1 z-50 "
          <> "opacity-0 group-hover/nav:opacity-100 transition-opacity duration-150 "
          <> if isTeacher
            then "pointer-events-none group-hover/nav:pointer-events-auto"
            else "pointer-events-none group-hover/nav:delay-300"
    ]
    [ MH.div_
        [class_ "min-w-56 bg-popover text-popover-foreground border border-border rounded-md shadow-lg p-3"]
        ( [ MH.div_ [class_ "font-semibold text-sm"] [M.text cat.categoryLabel]
          , MH.div_ [class_ "text-xs text-muted-foreground mt-1"] [M.text cat.categoryDescription]
          ]
            <> teacherSubEntries
        )
    ]
  where
    teacherSubEntries
      | not isTeacher || null cat.subEntries = []
      | otherwise =
          HoverMenu.hoverMenuSeparator
            : map (tooltipEntry navigate currentPage) cat.subEntries

-- | Clickable sub-entry within a teacher tooltip.
tooltipEntry :: (Page -> a) -> Page -> NavEntry -> View m a
tooltipEntry navigate currentPage entry =
  Button.toggleGhostSm
    (currentPage == entry.entryPage)
    (Button.ButtonConfig (Button.IconText entry.entryIcon entry.entryLabel) (Just (navigate entry.entryPage)))

-- | Burger menu as a CSS-only hover dropdown (replaces overlay sidebar).
burgerMenuView :: (Page -> a) -> Page -> [NavCategory] -> [(MisoString, [NavEntry])] -> View m a
burgerMenuView navigate currentPage cats extras =
  HoverMenu.hoverMenu
    burgerTrigger
    (intercalate [HoverMenu.hoverMenuSeparator] allGroups)
  where
    burgerTrigger =
      MH.div_
        [class_ "p-1 rounded-md hover:bg-white/10 text-primary-foreground cursor-pointer"]
        [Icon.iconS Icon.XLarge Icon.IcnMenu]
    allGroups =
      map (renderBurgerCategory navigate currentPage) cats
        <> map (renderExtraCategory navigate currentPage) extras

-- | Render a category in the burger menu (heading + main entry + sub-entries).
renderBurgerCategory :: (Page -> a) -> Page -> NavCategory -> [View m a]
renderBurgerCategory navigate currentPage cat =
  [ HoverMenu.hoverMenuHeading cat.categoryLabel
  , HoverMenu.hoverMenuEntry
      (currentPage == cat.categoryPage)
      cat.categoryIcon
      cat.categoryLabel
      (navigate cat.categoryPage)
  ]
    <> map (renderSubEntry navigate currentPage) cat.subEntries

-- | Render an extra category in the burger menu.
renderExtraCategory :: (Page -> a) -> Page -> (MisoString, [NavEntry]) -> [View m a]
renderExtraCategory navigate currentPage (heading, entries) =
  HoverMenu.hoverMenuHeading heading
    : map (renderSubEntry navigate currentPage) entries

-- | Render a sub-entry in a hover dropdown or burger menu.
renderSubEntry :: (Page -> a) -> Page -> NavEntry -> View m a
renderSubEntry navigate currentPage entry =
  HoverMenu.hoverMenuEntry
    (currentPage == entry.entryPage)
    entry.entryIcon
    entry.entryLabel
    (navigate entry.entryPage)

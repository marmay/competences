# Basecoat Component Reference

This document extracts Tailwind CSS class patterns from Basecoat UI components for use in our Haskell/Miso View modules.

**Source:** [Basecoat UI](https://basecoatui.com/) | [GitHub](https://github.com/hunvreus/basecoat)
**Extracted from:** `src/css/basecoat.css`

## Design Tokens

### Spacing Scale
- `gap-1`, `gap-1.5`, `gap-2`, `gap-3`, `gap-6`
- `px-1`, `px-2`, `px-3`, `px-4`, `px-6`
- `py-0.5`, `py-1`, `py-1.5`, `py-2`, `py-3`, `py-6`

### Border Radius
- `rounded-sm` - Small radius (4px)
- `rounded-md` - Medium radius (6px)
- `rounded-lg` - Large radius (8px)
- `rounded-xl` - Extra large radius (12px)
- `rounded-full` - Fully rounded

### Shadows
- `shadow-xs` - Extra small shadow
- `shadow-sm` - Small shadow
- `shadow-md` - Medium shadow
- `shadow-lg` - Large shadow

### Focus Ring
- `focus-visible:ring-[3px]` - Standard focus ring (3px width)
- `focus-visible:outline-none` - Remove default outline

### Colors (CSS Variables)
Basecoat uses CSS custom properties for theming:
- `bg-primary`, `text-primary-foreground`
- `bg-secondary`, `text-secondary-foreground`
- `bg-destructive`, `text-white` (destructive always white text)
- `bg-accent`, `text-accent-foreground`
- `bg-muted`, `text-muted-foreground`
- `bg-card`, `text-card-foreground`
- `bg-background`, `text-foreground`
- `border-input`

**For our implementation:** Map these to concrete Tailwind colors (sky, stone, red, etc.)

---

## Button Component

### Base Classes (All Buttons)
```
inline-flex items-center justify-center
text-sm font-medium
rounded-md
focus-visible:ring-[3px]
disabled:opacity-50
```

### Variants

**Primary** (`.btn-primary`):
```
bg-primary text-primary-foreground
hover:bg-primary/90
```
*Map to:* `bg-sky-600 text-white hover:bg-sky-700`

**Secondary** (`.btn-secondary`):
```
bg-secondary text-secondary-foreground
hover:bg-secondary/80
```
*Map to:* `bg-stone-200 text-stone-900 hover:bg-stone-300`

**Destructive** (`.btn-destructive`):
```
bg-destructive text-white
dark:bg-destructive/60
```
*Map to:* `bg-red-600 text-white hover:bg-red-700`

**Outline** (`.btn-outline`):
```
border bg-background
hover:bg-accent
```
*Map to:* `border border-stone-300 bg-white hover:bg-stone-100`

**Ghost** (`.btn-ghost`):
```
hover:bg-accent text-accent-foreground
```
*Map to:* `hover:bg-stone-100`

**Link** (`.btn-link`):
```
text-primary
underline-offset-4 hover:underline
```
*Map to:* `text-sky-600 underline-offset-4 hover:underline`

### Sizes

**Default** (`.btn`):
```
h-9 px-4 py-2 gap-2
```

**Small** (`.btn-sm`):
```
h-8 px-3 gap-1.5
```

**Large** (`.btn-lg`):
```
h-10 px-6 gap-2
```

**Icon Only** (`.btn-icon`, `.btn-sm-icon`, `.btn-lg-icon`):
```
size-9    (default icon)
size-8    (small icon)
size-10   (large icon)
```

---

## Input Components

### Text Input
```
appearance-none
h-9 w-full
rounded-md border
bg-transparent
px-3 py-1
shadow-xs
focus-visible:ring-[3px]
disabled:opacity-50 disabled:cursor-not-allowed
placeholder:text-muted-foreground
selection:bg-primary
```

### Textarea
```
border-input
h-16 w-full
rounded-md border
bg-transparent
px-3 py-2
focus-visible:ring-[3px]
placeholder:text-muted-foreground
disabled:opacity-50
```

### Label
```
flex items-center gap-2
text-sm leading-none font-medium
select-none
peer-disabled:opacity-50 peer-disabled:pointer-events-none
```

### Field Wrapper
```
flex flex-col w-full gap-3
data-[invalid=true]:text-destructive
```

Horizontal variant:
```
[data-orientation='horizontal']: flex-row items-center
```

---

## Card Component

### Base Card
```
bg-card text-card-foreground
flex flex-col gap-6
rounded-xl border
py-6
shadow-sm
```

### Card Sections

**Header** (`> header`):
```
grid grid-rows-[auto_auto] items-start gap-1.5
px-6
```

**Content** (`> section`):
```
px-6
```

**Footer** (`> footer`):
```
flex items-center
px-6
```

### Usage Pattern
```html
<div class="card">
  <header>
    <h2>Title</h2>
    <p>Description</p>
  </header>
  <section>
    Content
  </section>
  <footer>
    Actions
  </footer>
</div>
```

---

## Badge Component

### Base Classes (All Badges)
```
inline-flex items-center justify-center
rounded-full border
px-2 py-0.5
text-xs font-medium
focus-visible:border-ring focus-visible:ring-[3px]
```

### Variants

**Primary** (`.badge`, `.badge-primary`):
```
bg-primary text-primary-foreground
```
*Map to:* `bg-sky-600 text-white`

**Secondary** (`.badge-secondary`):
```
bg-secondary text-secondary-foreground
```
*Map to:* `bg-stone-200 text-stone-900`

**Destructive** (`.badge-destructive`):
```
bg-destructive text-white
dark:bg-destructive/60
```
*Map to:* `bg-red-600 text-white`

**Outline** (`.badge-outline`):
```
text-foreground
hover:bg-accent text-accent-foreground
```
*Map to:* `border-stone-300 text-stone-900 hover:bg-stone-100`

---

## Table Component

### Base Table
```
w-full caption-bottom text-sm
```

### Table Elements

**Table Head Row** (`thead [&_tr]`):
```
border-b
```

**Table Body Row** (`tbody [&_tr]`):
```
hover:bg-muted/50 border-b

Last row (tbody [&_tr:last-child]):
  border-0
```

**Table Footer** (`tfoot`):
```
bg-muted/50 border-t font-medium
```

**Table Header Cell** (`th`):
```
h-10 px-2
text-left font-medium
```

**Table Data Cell** (`td`):
```
p-2 align-middle
```

---

## Modal/Dialog Component

### Dialog Container (`.dialog`)
```
inset-y-0
opacity-0
transition-all transition-discrete

When open:
  opacity-100 scale-100
```

### Dialog Content
```
fixed top-[50%] left-[50%]
-translate-x-1/2 -translate-y-1/2
z-50
flex flex-col
max-w-lg
max-h-[calc(100%_-_2rem)]
```

---

## Alert Component

### Base Alert
```
relative w-full
rounded-lg border
px-4 py-3
text-sm
grid
has-[>svg]:grid-cols-[calc(var(--spacing)*4)_1fr]
items-start
[&>svg]:size-4
```

### Variants

**Default** (`.alert`):
```
bg-card text-card-foreground
```

**Destructive** (`.alert-destructive`):
```
text-destructive bg-card
```

---

## Checkbox Component

```
appearance-none
border-input
size-4
rounded-[4px]
border
shadow-xs

checked:bg-primary checked:border-primary
focus-visible:ring-[3px]
```

---

## Radio Component

```
appearance-none
size-4
rounded-full
border
shadow-xs
border-input text-primary
focus-visible:ring-[3px]

checked:before:
  size-2 bg-primary (center dot)
```

---

## Switch Component

```
appearance-none
h-[1.15rem] w-8
rounded-full
border border-transparent
shadow-xs
bg-input dark:bg-input/80

checked:bg-primary

before: (toggle handle)
  size-4 rounded-full bg-background
  checked:ms-3.5 (moves right when checked)
```

---

## Tabs Component

### Tab Container
```
flex flex-col gap-2
```

### Tab List (`[role='tablist']`)
```
bg-muted
inline-flex h-9 w-fit
rounded-lg
p-[3px]
```

### Individual Tab (`[role='tab']`)
```
inline-flex items-center justify-center
rounded-md
h-[calc(100%_-_1px)]

When selected ([aria-selected='true']):
  bg-background shadow-sm
```

---

## Select/Dropdown Component

### Native Select
```
appearance-none
border bg-transparent
h-9 px-3 pr-9 py-2
focus-visible:ring-[3px]
```
Note: Includes chevron background image

### Custom Select Options (`[role='option']`)
```
flex items-center gap-2
rounded-sm
pl-2 py-1.5 pr-7.5

When selected ([aria-selected='true']):
  Includes check-icon background
```

---

## Tooltip Component

### Tooltip Container (`[data-tooltip]`)
```
relative

::before:
  content-[attr(data-tooltip)]
  bg-primary text-primary-foreground
  rounded-md
  px-3 py-1.5
  text-xs
  invisible opacity-0 scale-95

hover:before:
  visible opacity-100 scale-100
```

### Position Variants
- **Top** (default): `before:bottom-full before:mb-1.5`
- **Bottom**: `before:top-full before:mt-1.5`
- **Left**: `before:right-full before:mr-1.5`
- **Right**: `before:left-full before:ml-1.5`

---

## Color Mapping for Implementation

Map Basecoat CSS variables to concrete Tailwind colors:

| Basecoat Variable | Tailwind Color | Use Case |
|-------------------|----------------|----------|
| `primary` | `sky-600` | Primary actions, links |
| `primary-foreground` | `white` | Text on primary background |
| `secondary` | `stone-200` | Secondary actions |
| `secondary-foreground` | `stone-900` | Text on secondary background |
| `destructive` | `red-600` | Destructive/delete actions |
| `accent` | `stone-100` | Hover states, subtle highlights |
| `accent-foreground` | `stone-900` | Text on accent background |
| `muted` | `stone-100` | Muted backgrounds |
| `muted-foreground` | `stone-500` | Muted text, placeholders |
| `card` | `white` | Card backgrounds |
| `card-foreground` | `stone-900` | Text on cards |
| `background` | `white` | Page background |
| `foreground` | `stone-900` | Default text color |
| `border` / `input` | `stone-300` | Borders, input borders |

---

## Implementation Notes

1. **Focus Ring**: Consistently use `focus-visible:ring-[3px]` for keyboard navigation
2. **Disabled State**: Use `disabled:opacity-50 disabled:pointer-events-none` or `disabled:cursor-not-allowed`
3. **Transitions**: Add `transition-colors` for smooth color changes on hover
4. **Dark Mode**: Basecoat uses dark mode variants (not implemented in our initial version)
5. **Spacing**: Use consistent gap values (1.5, 2, 3, 6) from spacing scale
6. **Border Radius**: Prefer `rounded-md` for most components, `rounded-lg` for cards, `rounded-full` for badges

---

## Quick Reference: Common Patterns

### Button
```
inline-flex items-center justify-center
rounded-md font-medium
transition-colors
focus-visible:outline-none focus-visible:ring-2
disabled:pointer-events-none disabled:opacity-50
```

### Input Field
```
rounded-md border border-stone-300
h-9 px-3
focus-visible:ring-2 focus-visible:ring-sky-600
```

### Card
```
rounded-xl border border-stone-200
bg-white shadow-sm
p-6
```

### Layout Container
```
flex flex-col gap-4
```

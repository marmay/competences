# Markdown Reference

This is the markup language used in task content, lesson descriptions, assignments, and other rich-text fields. The editor provides live preview with validation.

## Quick Example

```
# Lineare Gleichungen

Löse die folgenden Gleichungen. Beachte, dass $x \in \mathbb{R}$ gilt.

$$
2x + 5 = 3x - 7
$$

a. Bestimme die **Lösungsmenge** für $x$.
b. Überprüfe dein Ergebnis durch *Einsetzen*.
c. Formuliere einen vollständigen Antwortsatz.

> [!merksatz] Äquivalenzumformungen
> Bei Äquivalenzumformungen gilt: Was man auf der **einen Seite** macht,
> muss man auch auf der **anderen Seite** machen.
>
> Erlaubte Operationen: $+$, $-$, $\cdot$, $\div$ (außer durch $0$).

---

Viel Erfolg!
```

## Block Elements

### Paragraphs

Paragraphs are separated by blank lines. A single newline within a paragraph produces a space (soft break), not a new paragraph.

```
This is one paragraph.

This is another paragraph.
```

### Headings

Use `#` through `######` for heading levels 1–6. A space after the `#` markers is required.

```
# Heading 1
## Heading 2
### Heading 3
#### Heading 4
##### Heading 5
###### Heading 6
```

### Lists

Three kinds of lists are supported. Lists are **flat** — nested sub-lists are not supported. Items contain inline content only. Indented continuation lines extend the current item.

**Ordered lists** — use `1.`, `2.`, etc. The start number is preserved.

```
1. First item
2. Second item
3. Third item with a
   continuation line
```

**Bullet lists** — use `-`, `*`, or `+` followed by a space.

```
- Item one
- Item two
- Item three
```

**Lettered lists** — use `a.`, `b.`, `c.`, etc. (lowercase only). This is an extension for subtask numbering.

```
a. Berechne den Flächeninhalt.
b. Bestimme den Umfang.
c. Zeichne das Dreieck
   maßstabsgetreu.
```

### Tables

GFM-style pipe tables. A header row, a separator row that declares per-column alignment, and zero or more body rows.

```
| Name      | Alter | Beruf      |
|-----------|------:|:----------:|
| Anna      |    23 | Lehrerin   |
| Bernd     |    45 | Tischler   |
```

**Alignment** (separator row, per column):

- `---` — default (left)
- `:---` — left
- `---:` — right
- `:---:` — centered

At least three dashes per cell are required.

**Leading and trailing pipes are optional.** `a | b` and `| a | b |` are equivalent. Whitespace around cell content is trimmed.

**Cells contain inline content only** — text, bold, italic, inline code, links, file embeds, **inline math**, and **cloze blanks**. Block-level content (lists, math blocks, paragraphs) does not work inside a cell. If you need that, use a [`columns`](#columns-side-by-side) block instead.

**Inline math in cells** typesets naturally. Use this for value tables and any table that mixes numeric and symbolic content:

```
| $x$ | $f(x) = x^2$ |
|----:|-------------:|
|  -2 |          $4$ |
|  -1 |          $1$ |
|   0 |          $0$ |
```

**Cloze blanks in cells** turn a table into a fill-in-the-values exercise. Wrap inside a `task:cloze` block so the editor recognises it as a cloze task:

````
```task:cloze
Vervollständige die Wertetabelle:

| $x$ | $f(x)$  |
|----:|--------:|
|  -2 | ___2___ |
|  -1 |     ___ |
|   0 |     ___ |
```
````

**Literal pipes** inside a cell must be escaped: `\|`. **Pipes inside `$...$`, `\(...\)`, and `` `...` `` spans need no escape** — they are treated as part of the math or code, not as cell separators:

```
| Operator | Bedeutung |
|----------|-----------|
| $x \| y$ | x oder y  |
| `a\|b`   | Pipe-Symbol |
```

**Row width** — every body row must have the same number of cells as the header. The editor reports a validation error like *Tabelle, Zeile 2: Erwartet 3 Spalten, 2 gefunden.* if a row doesn't match.

#### Styling and layout

The table style is **fixed** and intentionally minimal:

- Rounded outer border, light header background, thick header underline.
- Light horizontal row dividers; **no vertical lines** (matches typeset textbook style, works for both general data tables and t-chart value tables).
- Per-column horizontal alignment as declared in the separator row.

**You cannot control via syntax:** cell borders, vertical column dividers, alternating row colours, header background, font size, per-cell styling, or column widths.

**Table width** is always 100% of the container — there is no per-table width attribute. To make a table narrower (or to put a plot next to it), wrap it in a [`columns`](#columns-side-by-side) block with a narrow weight:

````
```columns 1:2
| $x$ | $y$ |
|----:|----:|
|  -2 |   4 |
|   0 |   0 |
|   2 |   4 |
+++
The function $f(x) = x^2$ is the canonical example of a parabola opening
upward, with its vertex at the origin and axis of symmetry along the
$y$-axis.
```
````

This is the **only** mechanism for influencing table width — there are no `{width=...}` or similar attributes.

### Display Math

For standalone math formulas, use `$$...$$` or `\[...\]`. The content is rendered via MathJax.

```
$$
\int_0^1 x^2 \, dx = \frac{1}{3}
$$
```

Or equivalently:

```
\[
\int_0^1 x^2 \, dx = \frac{1}{3}
\]
```

### Fenced Code Blocks

Wrap code in triple backticks or triple tildes with an optional language tag.

````
```python
def hello():
    print("Hello, world!")
```
````

The language tags `geometry` and `svg` trigger special rendering — see [Special Code Blocks](#special-code-blocks) below.

### Admonitions

Callout boxes for definitions, theorems, remarks, and similar. Start the first line with `> [!type]` and optionally a title. Continue the body with lines prefixed by `>`.

```
> [!definition] Quadratische Gleichung
> Eine Gleichung der Form $ax^2 + bx + c = 0$ mit $a \neq 0$
> heißt **quadratische Gleichung**.
```

For blank lines within an admonition, place a `>` on the blank line:

```
> [!proof]
> Sei $n$ eine natürliche Zahl.
>
> Dann gilt $n + 1 > n$. $\square$
```

The body is recursively parsed — it can contain math, lists, and other block elements.

**Available types:**

| Type keyword | Alias | Display label |
|---|---|---|
| `definition` | — | Definition |
| `theorem` | `satz` | Satz |
| `lemma` | — | Lemma |
| `proof` | `beweis` | Beweis |
| `remark` | `bemerkung` | Bemerkung |
| `merksatz` | `remember` | Merksatz |
| `example` | `beispiel` | Beispiel |

Type keywords are case-insensitive. An unrecognized type falls back to Bemerkung.

### Thematic Breaks

A horizontal rule. Use three or more `-`, `*`, or `_` on a line by themselves.

```
---
```

## Inline Elements

### Bold and Italic

Use `**double asterisks**` for **bold** and `*single asterisks*` for *italic*. Nesting works: `***bold and italic***`. Only asterisk syntax is supported (not underscores).

```
This is **bold**, this is *italic*, and this is ***both***.
```

### Inline Code

Wrap text in single backticks for inline code: `` `code` ``.

```
Die Variable `x` wird substituiert.
```

### Inline Math

Use `$...$` or `\(...\)` for inline math formulas. Rendered via MathJax.

```
Die Lösung ist $x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$.
```

Or equivalently:

```
Die Lösung ist \(x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}\).
```

### Links

Use `[text](url)` or `[text](url "title")`. The link text supports inline formatting (bold, italic, code, math).

```
Siehe [Wikipedia](https://de.wikipedia.org/wiki/Quadratische_Gleichung).
Mehr Infos unter [GeoGebra](https://www.geogebra.org "Interaktive Mathematik").
```

### Line Breaks

A single newline within a paragraph produces a space (soft break). For a hard line break, end the line with a backslash `\` or two or more trailing spaces.

```
First line\
Second line (hard break)

First line
Second line (soft break — rendered as a space)
```

## Special Code Blocks

### Geometry Diagrams

Fenced code blocks with the `geometry` language tag render interactive 2D geometry diagrams. These support coordinate geometry, constructions, labeled points and segments, and more.

````
```geometry
defPoint A (0, 0)
defPoint B (4, 0)
drawSegment A -- B labeled "$c$" below
```
````

For the full geometry language reference, see [GEOMETRY-DSL.md](GEOMETRY-DSL.md).

### Inline SVG

Fenced code blocks with the `svg` language tag render the SVG source directly as a sandboxed image.

````
```svg
<svg viewBox="0 0 100 100">
  <circle cx="50" cy="50" r="40" fill="skyblue" />
</svg>
```
````

### Notes Grid

Fenced code blocks with the `btc:notes-grid` info string render a 2×2 grid of content cells. Cells are separated by `---` (three or more dashes on a line by themselves).

````
```btc:notes-grid
Cell 1
---
Cell 2
---
Cell 3
---
Cell 4
```
````

The four cells are laid out as: top-left, top-right, bottom-left, bottom-right. The top row has a light background (`bg-stone-50`), and cells are separated by subtle borders.

Each cell supports full block-level markdown — paragraphs, lists, headings, math, code blocks, and admonitions all work inside cells:

````
```btc:notes-grid
**Gegeben**

$a = 3$, $b = 4$
---
**Gesucht**

Hypotenuse $c$
---
**Lösung**

$$c = \sqrt{a^2 + b^2} = 5$$
---
**Antwort**

Die Hypotenuse ist $c = 5$ Einheiten lang.
```
````

If fewer than four cells are provided, the remaining cells are left empty:

````
```btc:notes-grid
Left column
---
Right column
```
````

This produces a grid with the bottom two cells empty.

### Columns (Side-by-Side)

Fenced code blocks with the `columns` info string lay out their cells side by side. Cells are separated by `+++` (three or more `+` on a line). This is the primary tool for plot-plus-table or explanation-plus-summary layouts.

````
```columns
![Graph](file:parabola.svg)
+++
| $x$ | $y$ |
|----:|----:|
|  -2 |   4 |
|  -1 |   1 |
|   0 |   0 |
|   1 |   1 |
|   2 |   4 |
```
````

Each cell supports **full block-level markdown** — paragraphs, lists, headings, math blocks, tables, file embeds, admonitions, even nested `columns` blocks.

#### Width ratios

Optional. Use `columns N:M:...` where each positive integer is a flex-grid weight (rendered as CSS `Nfr`). Default is an even split.

````
```columns 2:1
Two-thirds of the available width: an explanation with the **key concept**
worked through in detail, including derivations and worked examples.
+++
One-third: a quick reference card or summary box.
```
````

**Rules:**

- `columns` (no ratio) — equal split based on cell count.
- `columns 1:1`, `columns 2:1`, `columns 3:2:1`, `columns 1:1:1` — explicit weights.
- If fewer ratios than cells are provided, missing ratios default to `1`.
- If more ratios than cells, the extras are ignored.

Two- and three-column layouts are the common cases. The syntax accepts any number of columns.

#### Common patterns

**Plot + value table:**

````
```columns 1:1
![](file:graph.svg)
+++
| $x$ | $y$ |
|----:|----:|
|  -1 |   1 |
|   0 |   0 |
|   1 |   1 |
```
````

**Side-by-side comparison:**

````
```columns
**Methode A**

Schritt-für-Schritt mit Äquivalenzumformungen.
+++
**Methode B**

Grafische Lösung durch Schnittpunkte.
```
````

**Composition with cloze tasks:** `columns` works inside a `task:cloze` body, and a `task:cloze` body can contain `columns` — both compose recursively. A cloze exercise with a plot on one side and a fill-in value table on the other is one block, not two.

## Unsupported Features

The following standard markdown features are **not** available:

- Images (`![alt](url)`)
- Strikethrough (`~~text~~`)
- Task lists / checkboxes (`- [ ] item`)
- Footnotes
- Reference-style links (`[text][ref]`)
- Auto-links (`<url>`)
- Raw HTML
- Setext headings (underline style)
- Indented code blocks (use fenced blocks instead)
- Underscore emphasis (`_italic_`, `__bold__`)
- Nested lists
- Plain blockquotes (only admonition-style `> [!type]` is supported)

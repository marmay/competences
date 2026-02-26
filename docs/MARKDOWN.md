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

## Unsupported Features

The following standard markdown features are **not** available:

- Images (`![alt](url)`)
- Tables
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

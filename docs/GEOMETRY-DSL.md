# Geometry DSL — V1 Language Reference

A DSL for describing 2D geometry diagrams, embedded in ` ```geometry ` fenced code blocks. Designed for math education — coordinate geometry, constructions, labeled diagrams.

## Quick Example

```
defPoint A (0, 0)
defPoint B (4, 0)
defPoint C (2, 3)
defPointBy M (midpoint A B)

defSegment c A -- B

@axes, @grid {
  @labelAll above-right {
    drawPoint A
    drawPoint B
    drawPoint C
    drawPoint M
  }

  drawSegment c labeled "$c$" below
  drawSegment A -- C
  drawSegment B -- C

  @dashed {
    drawSegment M -- C
  }

  labelSegment B -- C "$a$" above
}
```

## Design: Three Orthogonal Operations

Every geometric primitive supports three independent operations:

| Operation | Purpose | Example |
|-----------|---------|---------|
| `def*` | Name a primitive for later reference | `defPoint A (1, 2)` |
| `draw*` | Render a primitive visually | `drawPoint A` |
| `label*` | Place a text annotation | `labelPoint A "A" above` |

**Inline specs** allow unnamed primitives: `drawSegment A -- B` draws a segment without naming it.

**`labeled` suffix** combines draw + label: `drawPoint A labeled "A" below-left`.

## Commands

### Definition Commands

```
defPoint <name> (<x>, <y>)
```
Define a named point at coordinates. Example: `defPoint A (3, -1.5)`

```
defPointBy <name> (<construction>)
```
Define a point via construction. Example: `defPointBy M (midpoint A B)`

```
defSegment <name> <pointA> -- <pointB>
```
Define a named segment. Example: `defSegment c A -- B`

### Draw Commands

```
drawPoint <name>
drawPoint <name> labeled "<text>" <position>
```
Render a point as a filled dot. Optional `labeled` suffix adds a text label.

```
drawSegment <name>
drawSegment <pointA> -- <pointB>
drawSegment <name-or-inline> labeled "<text>" <side> [<fraction>]
```
Render a segment (named or inline between two points). Optional `labeled` suffix.

```
drawAngle <A> <B> <C>
drawAngle <A> <B> <C> labeled "<text>"
drawAngle <A> <B> <C> labeled "<text>" +(<dx>, <dy>)
```
Draw an angle arc at vertex B, sweeping from ray BA to ray BC. Always shows the shorter angle (sweep normalized to ≤180°). Arc radius is `min(1.0, 0.5 * min(dist(B,A), dist(B,C)))`.

Optional `labeled` suffix places a label along the angle bisector. With `+(dx, dy)` offset, the label is placed externally relative to the vertex, with a leader line from the bisector position.

```
drawRightAngle <A> <B> <C>
```
Draw a right-angle marker at vertex B — German style: arc + dot at 50% radius. Uses a smaller arc radius (`min(0.7, 0.5 * min(dist(B,A), dist(B,C)))`). Does not support the `labeled` suffix; use `labelAngle` separately if needed.

```
drawPoly <vertex> [<decorations>] -- <vertex> [<decorations>] -- ... [close]
```
Draw a polygon with per-vertex and per-edge decorations. See [Polygon Commands](#polygon-commands) for full syntax.

### Label Commands

```
labelPoint <name> "<text>" <position>
```
Place text at a point. Positions: `above`, `below`, `left`, `right`, `above-left`, `above-right`, `below-left`, `below-right`.

```
labelSegment <name> "<text>" <side> [<fraction>]
labelSegment <pointA> -- <pointB> "<text>" <side> [<fraction>]
```
Label a segment. `<side>` is `above`/`left` or `below`/`right` (relative to direction A→B). `left`/`right` are synonyms that read more naturally for vertical segments. `<fraction>` (default 0.5) positions along the segment: 0.0 = at A, 1.0 = at B.

```
labelAngle <A> <B> <C> "<text>"
labelAngle <A> <B> <C> "<text>" +(<dx>, <dy>)
```
Place a label at an angle without drawing the arc. `A B C` is the angle reference where B is the vertex. The label is placed along the angle bisector by default. With `+(dx, dy)`, the label is placed externally relative to the vertex, with a leader line from the bisector position.

### `labeled` Suffix

Draw commands support `labeled` to combine drawing and labeling:

| Syntax | Equivalent to |
|--------|---------------|
| `drawPoint A labeled "A" below` | `drawPoint A` + `labelPoint A "A" below` |
| `drawSegment c labeled "c" below` | `drawSegment c` + `labelSegment c "c" below` |
| `drawSegment A -- B labeled "c" below 0.4` | `drawSegment A -- B` + `labelSegment A -- B "c" below 0.4` |
| `drawAngle A B C labeled "$\alpha$"` | `drawAngle A B C` + `labelAngle A B C "$\alpha$"` |

The parser desugars `labeled` into separate `Draw` + `Label` commands. The AST and evaluator never see it.

### Math Labels

Wrap label text in `$...$` to render it as LaTeX math via MathJax:

```
labelPoint A "$\alpha$" above
drawSegment c labeled "$c$" below
labelSegment B -- C "$a$" above
labelAngle A B C "$\alpha$"
```

Math labels work with all labeling forms: `labelPoint`, `labelSegment`, `labelAngle`, and the `labeled` suffix on draw commands.

If the closing `$` is missing or the content is empty (`$$`), the text is treated as a plain label.

## Polygon Commands

`drawPoly` draws a polygon with per-vertex and per-edge decorations:

```
drawPoly A [point "A"] -- B [angle "$\alpha$"] -- C [rightAngle] -- close
```

### Vertices

Vertices are point names or inline coordinates, optionally followed by decorations in `[...]`:

```
drawPoly A -- B -- C                       -- bare vertices, no decorations
drawPoly (0,0) -- (4,0) -- (2,3) -- close  -- inline coordinates
```

### Vertex Decorations

Square brackets after a vertex contain comma-separated decorations:

| Decoration | Arguments | Description |
|------------|-----------|-------------|
| `point` | `[text] [position]` | Draw dot + optional label |
| `angle` | `[text]` | Draw angle arc + optional label |
| `rightAngle` | (none) | Draw right-angle marker (arc + dot) |
| `label` | `text [position]` | Text label only, no dot |

Position is one of: `above`, `below`, `left`, `right`, `above-left`, `above-right`, `below-left`, `below-right`. When position is omitted on `point` or `label`, the label is auto-positioned along the outward bisector of the vertex.

Examples:
```
[point "A"]                  -- dot + label "A" (auto-positioned)
[point "A" below-left]       -- dot + label "A" at below-left
[angle "$\alpha$"]           -- angle arc + label
[rightAngle]                 -- right-angle marker
[label "A"]                  -- label only, no dot
[label "A" below]            -- label only, positioned below
[point "A", angle "$\alpha$"] -- multiple decorations
```

### Edge Decorations

Edges between vertices are `--` (bare) or `-[<decoration>]-`:

| Decoration | Arguments | Description |
|------------|-----------|-------------|
| `segment` | `text [side]` | Label the edge |

Side is `above`/`left` (→ above the edge) or `below`/`right` (→ below the edge).

Examples:
```
A -- B                          -- bare edge
A -[segment "$c$" below]- B    -- labeled edge
```

### Modifier Wrapping in Decorations

Decorations can be wrapped in modifier blocks using the `@` prefix:

```
[@color red { point "A" }]            -- red dot + label
[@dashed { segment "$c$" below }]     -- dashed edge label
[@color red { point "A" }, angle "$\alpha$"]  -- mixed
```

### Closing Polygons

End with `close` to connect the last vertex back to the first:

```
drawPoly A -- B -- C -- close    -- closed triangle (3 edges)
drawPoly A -- B -- C             -- open polygon (2 edges)
```

A polygon requires at least 3 vertices, or 2 vertices with `close`.

### Filled Polygons

Use `@fillColor` or `@palette` to fill the polygon interior:

```
@fillColor blue {
  drawPoly A -- B -- C -- close
}

@palette red {
  drawPoly A [point "A"] -- B [point "B"] -- C [point "C"] -- close
}
```

### Complete Example

```
defPoint A (0, 0)
defPoint B (4, 0)
defPoint C (2, 3)

@palette blue {
  drawPoly A [point "A" below-left] -[segment "$c$" below]- B [point "B" below-right, angle "$\\beta$"] -- C [point "C" above, rightAngle] -- close
}
```

## Point Constructions

Used with `defPointBy`:

```
defPointBy M (midpoint A B)          -- midpoint of A and B
defPointBy T (lerp A B 0.25)         -- 25% from A toward B
defPointBy R (rotate O 90 P)         -- rotate P around O by 90 degrees CCW
defPointBy Q (reflect (line A B) P)  -- reflect P across line through A, B
defPointBy S (translate (3, -1) P)   -- translate P by vector (3, -1)
```

| Construction | Syntax | Semantics |
|-------------|--------|-----------|
| `midpoint` | `midpoint <A> <B>` | Shorthand for `lerp A B 0.5` |
| `lerp` | `lerp <A> <B> <t>` | `A + t*(B-A)`. `t=0` gives A, `t=1` gives B |
| `rotate` | `rotate <center> <degrees> <point>` | Counter-clockwise rotation |
| `reflect` | `reflect (line <A> <B>) <point>` | Reflection across line |
| `translate` | `translate (<dx>, <dy>) <point>` | Vector translation |

## Modifier Blocks

Modifiers wrap commands in `{ }` and affect rendering within the block. All modifiers require the `@` prefix.

Multiple modifiers can be combined with commas before a single block:

```
@axes, @grid { ... }
@color red, @dashed { ... }
```

This is purely syntactic sugar — `@axes, @grid { ... }` desugars to `@axes { @grid { ... } }` in the parser.

### Environment Modifiers (change draw style)

#### Color Modifiers

All color modifiers take a [palette color name](#color-palettes):

| Modifier | Sets | Example |
|----------|------|---------|
| `@color <c>` | lineColor + textColor | `@color red { ... }` |
| `@lineColor <c>` | lineColor only | `@lineColor blue { ... }` |
| `@textColor <c>` | textColor only | `@textColor green { ... }` |
| `@fillColor <c>` | fillColor only (enables polygon fills) | `@fillColor orange { ... }` |
| `@figure <c>` | lineColor + fillColor | `@figure red { ... }` |
| `@palette <c>` | lineColor + textColor + fillColor | `@palette blue { ... }` |

#### Style Modifiers

```
@dashed { ... }        -- dashed line style
@thick { ... }         -- thick lines
@thin { ... }          -- thin lines
```

#### Sizing Modifiers

| Modifier | Default | Effect |
|----------|---------|--------|
| `@labelDist <n>` | 1.0 | Distance multiplier for all label offsets |
| `@fontSize <n>` | 0.45 | Font size in coordinate units |
| `@dotRadius <n>` | 0.10 | Dot radius for drawPoint |

Environment changes are scoped — restored after `}`.

### Auto-Decorating Modifiers

```
@labelAll <position> { ... }   -- auto-label every drawPoint with its name
@axes { ... }                  -- add coordinate axes to background
@grid { ... }                  -- add unit grid to background
```

- `@labelAll`: labels go to the **foreground** layer
- `@axes`/`@grid`: decorations go to the **background** layer (gray axes with integer tick marks, light-gray grid lines)

### Layer Modifiers

```
@background { ... }    -- route output to background layer
@foreground { ... }    -- route output to foreground layer
```

Default layer is `main`. Rendering order: background, then main, then foreground.

### Coordinate Transforms

```
@scale 2.0 { ... }       -- uniform scale around auto-centroid
@scale 2.0 O { ... }     -- uniform scale around named point O
```

- Applied post-hoc to all render output (coordinates and radii)
- Auto-centroid = average of all coordinates in the block
- Composable: nesting `@scale` blocks multiplies factors
- Does not scale presentation parameters (label offsets, font size)

### Nesting and Scoping

Modifiers nest freely:
```
@axes {
  @color blue {
    @dashed {
      drawSegment A -- B
    }
  }
}
```

**Flat namespace:** All `def*` commands define into a single global namespace regardless of nesting depth. A point defined inside a modifier block is visible everywhere:
```
@color red {
  defPoint P (1, 2)    -- P is globally visible
}
drawPoint P              -- OK
```

## Color Palettes

Five named color palettes are available:

| Name | Stroke/Line Color | Fill Color |
|------|-------------------|------------|
| `red` | Tailwind red-600 | Tailwind red-100 |
| `blue` | Tailwind blue-600 | Tailwind blue-100 |
| `green` | Tailwind green-600 | Tailwind green-100 |
| `orange` | Tailwind orange-600 | Tailwind orange-100 |
| `purple` | Tailwind purple-600 | Tailwind purple-100 |

Only these 5 names are valid palette colors. Using an unknown name produces a parse error listing the available options.

- Stroke/line colors use the `-600` shade (medium intensity, good for lines and text)
- Fill colors use the `-100` shade (light, good for polygon backgrounds)
- `@color` and `@lineColor` set the stroke color; `@textColor` sets text color; `@fillColor` sets the fill color
- `@palette` sets all three (line, text, fill) from the same named palette

## Three-Layer Rendering

Output is composited in three layers:

1. **Background** — axes, grid lines (drawn first, behind everything)
2. **Main** — user geometry (default layer)
3. **Foreground** — auto-generated labels from `@labelAll` (drawn last, on top)

Use `@background { }` and `@foreground { }` to route primitives explicitly.

## Architecture

```
Text ──→ Parser ──→ [Command] ──→ Eval ──→ RenderResult ──→ SVG Renderer
```

| Module | Purpose |
|--------|---------|
| `Competences.Markdown.Geometry.AST` | All types |
| `Competences.Markdown.Geometry.Parser` | `parseGeometry :: Text -> Either Error [Command]` |
| `Competences.Markdown.Geometry.Eval` | `evalScene :: [Command] -> RenderResult` |
| `Competences.Markdown.Geometry.Palette` | Named color palettes (stroke + fill resolution) |
| `Competences.Frontend.Component.Geometry` | `renderGeometryText :: Text -> View` |

## Extension Path

V1 implements points, segments, angles, and polygons. Future versions add primitives following the same patterns.

### Adding a New Primitive (e.g., Circle)

1. **AST** — add constructors:
   ```haskell
   data SegmentRef = ... | CircleByName !Name | CircleInline !Name !Double

   data DrawPrimitive = ... | DrawCircle !CircleRef
   data LabelPrimitive = ... | LabelOnCircle !CircleRef !Text !Double  -- angle

   data Command = ... | DefCircle !Name !Name !Double  -- name center radius
   ```

2. **Parser** — add keyword cases:
   ```haskell
   "defCircle"  -> one <$> defCircleP
   "drawCircle" -> drawCircleP
   ```

3. **Eval** — add state map + eval cases:
   ```haskell
   -- In EvalState:
   esCircles :: Map Name (Name, Double)  -- name -> (center, radius)

   -- One new case in evalDraw, one in evalLabel
   ```

4. **Renderer** — add `RenderCircle` primitive and SVG case.

### Planned Extensions

- **Circles and arcs:** `defCircle`, `defArc`, `drawCircle`, `drawArc`
- **Intersection constructions:** `intersect1 g k`, `intersect2 g k`
- **Decoration marks:** `markEqual 1 { }`, `markParallel 1 { }`

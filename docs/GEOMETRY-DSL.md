# Geometry DSL — V1 Language Reference

A DSL for describing 2D geometry diagrams, embedded in ` ```geometry ` fenced code blocks. Designed for math education — coordinate geometry, constructions, labeled diagrams.

## Quick Example

```
defPoint A (0, 0)
defPoint B (4, 0)
defPoint C (2, 3)
defPointBy M (midpoint A B)

defSegment c A -- B

axes, grid {
  labelAll above-right {
    drawPoint A
    drawPoint B
    drawPoint C
    drawPoint M
  }

  drawSegment c labeled "$c$" below
  drawSegment A -- C
  drawSegment B -- C

  dashed {
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

### `labeled` Suffix

Draw commands support `labeled` to combine drawing and labeling:

| Syntax | Equivalent to |
|--------|---------------|
| `drawPoint A labeled "A" below` | `drawPoint A` + `labelPoint A "A" below` |
| `drawSegment c labeled "c" below` | `drawSegment c` + `labelSegment c "c" below` |
| `drawSegment A -- B labeled "c" below 0.4` | `drawSegment A -- B` + `labelSegment A -- B "c" below 0.4` |

The parser desugars `labeled` into separate `Draw` + `Label` commands. The AST and evaluator never see it.

### Math Labels

Wrap label text in `$...$` to render it as LaTeX math via MathJax:

```
labelPoint A "$\alpha$" above
drawSegment c labeled "$c$" below
labelSegment B -- C "$a$" above
```

Math labels work with all labeling forms: `labelPoint`, `labelSegment`, and the `labeled` suffix on draw commands.

If the closing `$` is missing or the content is empty (`$$`), the text is treated as a plain label.

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

Modifiers wrap commands in `{ }` and affect rendering within the block.

Multiple modifiers can be combined with commas before a single block:

```
axes, grid { ... }
color red, dashed { ... }
```

This is purely syntactic sugar — `axes, grid { ... }` desugars to `axes { grid { ... } }` in the parser.

### Environment Modifiers (change draw style)

```
color red { ... }     -- set stroke/fill color
dashed { ... }        -- dashed line style
thick { ... }         -- thick lines
thin { ... }          -- thin lines
```

Environment changes are scoped — restored after `}`.

### Auto-Decorating Modifiers

```
labelAll <position> { ... }   -- auto-label every drawPoint with its name
axes { ... }                  -- add coordinate axes to background
grid { ... }                  -- add unit grid to background
```

- `labelAll`: labels go to the **foreground** layer
- `axes`/`grid`: decorations go to the **background** layer

### Layer Modifiers

```
background { ... }    -- route output to background layer
foreground { ... }    -- route output to foreground layer
```

Default layer is `main`. Rendering order: background, then main, then foreground.

### Nesting and Scoping

Modifiers nest freely:
```
axes {
  color blue {
    dashed {
      drawSegment A -- B
    }
  }
}
```

**Flat namespace:** All `def*` commands define into a single global namespace regardless of nesting depth. A point defined inside a modifier block is visible everywhere:
```
color red {
  defPoint P (1, 2)    -- P is globally visible
}
drawPoint P              -- OK
```

## Three-Layer Rendering

Output is composited in three layers:

1. **Background** — axes, grid lines (drawn first, behind everything)
2. **Main** — user geometry (default layer)
3. **Foreground** — auto-generated labels from `labelAll` (drawn last, on top)

Use `background { }` and `foreground { }` to route primitives explicitly.

## Architecture

```
Text ──→ Parser ──→ [Command] ──→ Eval ──→ RenderResult ──→ SVG Renderer
```

| Module | Purpose |
|--------|---------|
| `Competences.Markdown.Geometry.AST` | All types |
| `Competences.Markdown.Geometry.Parser` | `parseGeometry :: Text -> Either Error [Command]` |
| `Competences.Markdown.Geometry.Eval` | `evalScene :: [Command] -> RenderResult` |
| `Competences.Frontend.Component.Geometry` | `renderGeometryText :: Text -> View` |

## Extension Path

V1 implements points and segments. Future versions add primitives following the same patterns.

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

- **V1.1:** Circles, arcs, angles (`defCircle`, `defArc`, `drawAngle`)
- **V1.x:** Intersection constructions (`intersect1 g k`, `intersect2 g k`)
- **V1.x:** Decoration marks (`markEqual 1 { }`, `markParallel 1 { }`)
- **V1.x:** Composite sugar (`drawTriangle A B C` desugars to three `drawSegment`)

Each follows the same mechanical pattern: one new constructor + one parser case + one eval case + one SVG case.

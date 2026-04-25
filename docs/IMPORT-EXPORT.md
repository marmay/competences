# Import & Export Reference

Round-trip format for teaching content (tasks, assignments, resources, lessons). YAML on the clipboard, encoded and decoded by the backend's `/api/exchange/{encode,decode}` endpoints. Designed to be readable and writeable by both humans and language models.

This document is the authoritative schema; if something here disagrees with the running code, the code wins and this file is wrong.

---

## Top-level structure

A document is a record of seven lists. **Every list is optional and defaults to empty** — omit the keys you don't need.

```yaml
competenceGrids: []    # competence schemas (carry their competences inline)
tasks: []              # standalone tasks (published pool)
draftTasks: []         # standalone tasks (draft pool)
assignments: []        # published assignments
draftAssignments: []   # draft assignments
resources: []
lessons: []
```

Single-entity exports populate just one or two lists. A lesson export populates every list the lesson transitively references. A grid export comes in two flavours: grid-only (just `competenceGrids`) and grid-with-content (also fills `tasks` and `resources` with everything that references the grid's competences).

**References between entities use names/identifiers, never IDs.**

- An assignment's `taskRefs` lists the `identifier` strings of tasks it owns; the bodies live in `tasks` (or `draftTasks`).
- A lesson's `assignmentRefs` lists assignment `name`s linked from this lesson.
- A lesson's phase / supplemental items reference by `kind` + `ref` string (assignment `name`, task `identifier`, or resource `identifier`). Resources reach a lesson exclusively via these item lists.

---

## Matching semantics on import

For each incoming entity the importer looks up an existing one by:

| Entity | Matching key |
|---|---|
| Competence grid | `title` |
| Competence (within a grid) | `description`, scoped to the matched grid |
| Task | `identifier`, scoped to draft/published pool |
| Assignment | `name`, scoped to draft/published pool |
| Resource | `identifier` |
| Lesson | `title` (across all meso plans) |

Match results in one of four actions:

- **Create** — no match found, entity is new.
- **Update** — match found, fields differ; existing entity is patched in place.
- **NoChange** — match found, fields identical; nothing happens.
- **Delete** — emitted only for competences that exist in the document but aren't in the imported grid. The backend rejects deletes of in-use competences (so an import that would orphan task/resource references will fail at apply time, with the error surfaced to the user).

For matched updates the **import is the source of truth**. Replaceable fields (content, attachments, the assignment's task list, the resource's competence levels, etc.) are overwritten with the imported values; previously-attached data not present in the import is dropped on Update. The preview shows the diff before Apply.

### Renames

Every entity payload accepts an optional `replaces` field carrying the **previous** matching key. When present, the matcher tries the old key first; if a match is found, the entity is treated as an Update and renamed to the current `name` / `identifier` / `title` / `description`. If the old key doesn't match either, the matcher falls back to looking up by the current key.

```yaml
tasks:
  - identifier: "Buch-3.46"
    replaces: "Buch-3.45"   # old identifier; matcher updates the existing task and renames it
    title: "..."
```

`replaces` is never populated by export — it's always written by hand (or by an LLM acting on user instructions). Omit it if you're not renaming.

### Conflicts (block Apply)

The importer surfaces a hard conflict when an inlined assignment is already linked to a lesson **not** in the import. Applying would silently steal the assignment from its existing lesson, so Apply is gated until the conflict is resolved (rename the imported assignment, or delete the existing one, then re-paste).

### Warnings (hold-to-confirm)

Soft warnings surface for ambiguous matches (e.g. multiple existing assignments share an imported name). Apply is gated behind an explicit "accept warnings" click rather than blocked outright.

### Apply order

`competenceGrids` → `tasks` → `resources` → `assignments` → `lessons`. Each phase resolves the previous phase's freshly-applied ids when wiring up references.

---

## Entity schemas

### Competence grid (`competenceGrids`)

```yaml
competenceGrids:
  - title: "Lineare Algebra"
    description: "Schemata für lineare Gleichungen und Vektorrechnung."
    competences:
      - description: "Gleichungen lösen"
        levels:
          BasicLevel: "Einfache Gleichungen mit einer Unbekannten lösen."
          IntermediateLevel: "Gleichungssysteme mit zwei Unbekannten lösen."
          AdvancedLevel: "Gleichungssysteme mit Matrizenrechnung lösen."
      - description: "Vektoren"
        replaces: "Vektorrechnung"
        levels:
          BasicLevel: "Vektoren addieren und subtrahieren."
          AdvancedLevel: "Kreuzprodukt berechnen und geometrisch interpretieren."
```

| Field (grid) | Type | Required | Notes |
|---|---|---|---|
| `title` | string | yes | Matching key. |
| `replaces` | string \| null | no | Previous title for renames. |
| `description` | string | yes | Free-form schema description. |
| `competences` | list | yes | The grid's competences, inline. |

| Field (competence) | Type | Required | Notes |
|---|---|---|---|
| `description` | string | yes | Matching key, scoped to its grid. |
| `replaces` | string \| null | no | Previous description for renames. |
| `levels` | map of Level → string | yes | Per-level description. Absent levels mean "no description at this level". |

**Competence list semantics on Update:** existing competences not present in the import are emitted as Deletes. The backend rejects deletes of in-use competences (referenced by tasks, resources, evidences, etc.), so an apply will fail loudly if the import would orphan references. Use `replaces` on a competence to rename it instead of inadvertently deleting + recreating it under a new name.

### Task (`tasks` / `draftTasks`)

```yaml
tasks:
  - identifier: "Buch-3.45"
    title: "Lineare Gleichung"
    content: |
      Löse die Gleichung $2x + 5 = 17$ und gib die Lösungsmenge an.
    purpose: Practice          # Practice | Assessment
    primary:
      - { grid: "Lineare Algebra", description: "Gleichungen lösen", level: BasicLevel }
    secondary: []
    solutions:
      - solutionType: Hint     # Hint | Results | Complete
        content: "Bringe alle Terme mit $x$ auf eine Seite."
      - solutionType: Results
        content: "$\\mathbb{L} = \\{6\\}$"
    attachments: []            # see "Attachments" section below
```

| Field | Type | Required | Notes |
|---|---|---|---|
| `identifier` | string | yes | Matching key. Must be unique within its pool. |
| `replaces` | string \| null | no | Previous identifier for renames. |
| `title` | string | yes | Human-readable title. |
| `content` | string \| null | no | Rich-text body. See "Rich-text content" below. |
| `purpose` | enum | yes | `Practice` (develops competence) or `Assessment` (proves it). |
| `primary` | list of competence refs | yes | Competences this task primarily tests. Empty list is fine. |
| `secondary` | list of competence refs | yes | Competences this task may also test. |
| `solutions` | list | yes | See solution shape below. Empty list means no solutions provided. |
| `attachments` | list | yes | See attachment shape below. |

**Solution replace-by-type:** on Update, any existing solution whose `solutionType` matches an imported one is overwritten; existing solutions of types not in the import are preserved. So a re-import that only ships a `Results` solution updates that one without touching an existing `Hint`.

### Assignment (`assignments` / `draftAssignments`)

```yaml
assignments:
  - name: "Mathematik-Test 3a"
    description: |
      Überprüfung zum Thema lineare Gleichungen.
    assignmentDate: 2026-02-15      # ISO date
    activityType: Exam              # see enum below
    groupSubmissionAllowed: false
    taskRefs: ["Buch-3.45", "Buch-3.46"]
```

| Field | Type | Required | Notes |
|---|---|---|---|
| `name` | string | yes | Matching key. |
| `replaces` | string \| null | no | Previous name for renames. |
| `description` | string | yes | Rich-text body. Empty string allowed. |
| `assignmentDate` | ISO date `YYYY-MM-DD` | yes | |
| `activityType` | enum | yes | `Conversation`, `Exam`, `SchoolExercise`, `HomeExercise`, `Correction`. |
| `groupSubmissionAllowed` | bool | yes | |
| `taskRefs` | list of strings | yes | Task identifiers. The bodies must appear in the top-level `tasks` (or `draftTasks` if the assignment is in `draftAssignments`) list, otherwise the reference drops silently on apply. |

The pool (`assignments` vs `draftAssignments`) decides draft/published; there is **no per-payload `isDraft` flag**.

### Resource (`resources`)

```yaml
resources:
  - identifier: "Arbeitsblatt-7"
    content:
      tag: ExInlineContent           # see content variants below
      contents: |
        Übungsaufgaben zum Thema **Bruchrechnung**. Bearbeite Aufgaben 1–5.
    competenceLevels:
      - { grid: "Lineare Algebra", description: "Gleichungen lösen", level: BasicLevel }
    attachments: []
```

| Field | Type | Required | Notes |
|---|---|---|---|
| `identifier` | string | yes | Matching key. |
| `replaces` | string \| null | no | Previous identifier for renames. |
| `content` | tagged sum | yes | One of four variants — see below. |
| `competenceLevels` | list of competence refs | yes | |
| `attachments` | list | yes | |

**Content variants** (tagged sum):

```yaml
# Inline rich text
{ tag: ExInlineContent, contents: "..." }

# Web link
{ tag: ExWebLink, contents: ["https://example.com", "Description"] }

# Video link
{ tag: ExVideoLink, contents: ["https://example.com/v", "Description"] }

# File-backed (resolves via the receiving instance's CAS by sha256)
{ tag: ExFileContent
, contents: { fileName: "...", mimeType: "...", sha256: "...", bytes: 12345 } }
```

### Lesson (`lessons`)

```yaml
lessons:
  - title: "Lineare Gleichungen einführen"
    description: |
      Einführung in das Lösen linearer Gleichungen.
    date: 2026-03-15                # nullable
    competences:
      - { grid: "Lineare Algebra", description: "Gleichungen lösen", level: BasicLevel }
    phases:
      - title: "Einstieg"
        socialForm: WholeClass      # see enum below
        duration: 10                # minutes
        actionForm: Presenting      # see enum below
        items:
          - { kind: ItemResource, ref: "Arbeitsblatt-7", publish: true }
      - title: "Erarbeitung"
        socialForm: SmallGroups
        duration: 20
        actionForm: Collaborating
        items:
          - { kind: ItemAssignment, ref: "Mathematik-Test 3a", publish: true }
    supplementalItems: []
    notesTitleOverride: null
    assignmentRefs: ["Mathematik-Test 3a"]
```

| Field | Type | Required | Notes |
|---|---|---|---|
| `title` | string | yes | Matching key. |
| `replaces` | string \| null | no | Previous title for renames. |
| `description` | string | yes | Rich text. Empty string allowed. |
| `date` | ISO date \| null | yes | |
| `competences` | list of competence refs | yes | |
| `phases` | list | yes | See phase shape below. |
| `supplementalItems` | list | yes | Same shape as phase `items`; rendered below the phase block. |
| `notesTitleOverride` | string \| null | yes | Override for the auto-derived student-facing title. |
| `assignmentRefs` | list of strings | yes | Assignment names linked to this lesson, in order. Resources reach a lesson via phase / supplemental items only. |

A new lesson lands in the **first** meso plan in the document. Updating an existing lesson preserves its meso plan and order.

#### Phase shape

```yaml
title: "Einstieg"
socialForm: WholeClass        # WholeClass | SmallGroups | PairWork | IndividualWork
duration: 10                  # integer minutes
actionForm: Presenting        # Presenting | Collaborating | Assigning
items:
  - { kind: ItemAssignment | ItemTask | ItemResource, ref: "<name or identifier>", publish: true }
```

`ref` strings are matched against the freshly-applied entities by the keys in the matching table above (`name` for assignments, `identifier` for tasks/resources). Items whose `ref` doesn't resolve drop silently — they're not loud failures because partial paste is a legitimate workflow.

---

## Shared sub-schemas

### Competence reference

```yaml
{ grid: "Lineare Algebra", description: "Gleichungen lösen", level: BasicLevel }
```

| Field | Type | Notes |
|---|---|---|
| `grid` | string | Grid title, matched case- and whitespace-insensitively. |
| `description` | string | Competence description, matched within the grid the same way. |
| `level` | enum | `BasicLevel`, `IntermediateLevel`, `AdvancedLevel`. |

Unmatched references appear in the preview with a "?" marker. They drop on apply rather than blocking — a teacher can fix the grid afterwards.

### Attachment

```yaml
{ fileName: "diagram.svg", mimeType: "image/svg+xml", sha256: "<64-hex>", bytes: 1234 }
```

| Field | Type | Notes |
|---|---|---|
| `fileName` | string | Display name. |
| `mimeType` | string | |
| `sha256` | string | Lowercase hex, length 64. The receiving instance resolves the blob from its CAS by this hash. |
| `bytes` | integer | File size in bytes. |

**Same-server only.** Cross-server imports leave attachments dangling because the sha256 won't resolve in the receiving CAS. Cross-server attachment support is planned (see `TODO.md`) and will extend this shape; the same-server schema above stays valid.

---

## Enum reference

All enums use their Haskell constructor names verbatim. Spellings are case-sensitive.

| Enum | Values |
|---|---|
| `purpose` (TaskPurpose) | `Practice`, `Assessment` |
| `solutionType` | `Hint`, `Results`, `Complete` |
| `level` | `BasicLevel`, `IntermediateLevel`, `AdvancedLevel` |
| `activityType` | `Conversation`, `Exam`, `SchoolExercise`, `HomeExercise`, `Correction` |
| `socialForm` (TeachingSocialForm) | `WholeClass`, `SmallGroups`, `PairWork`, `IndividualWork` |
| `actionForm` | `Presenting`, `Collaborating`, `Assigning` |
| `kind` (lesson item) | `ItemAssignment`, `ItemTask`, `ItemResource` |
| `tag` (resource content) | `ExInlineContent`, `ExWebLink`, `ExVideoLink`, `ExFileContent` |

---

## Rich-text content

Fields documented as "rich text" (task `content`, assignment `description`, lesson `description` and `notes`, solution `content`, inline resource content) are interpreted by the application's own markdown dialect — same syntax used in the editor. The dialect supports:

- standard CommonMark prose (paragraphs, bold/italic, lists, headings, code blocks, tables)
- inline math `$...$` and display math `$$...$$` (LaTeX/MathJax)
- callouts: blockquote-with-prefix syntax (`> [!note]`, `> [!tip]`, etc.)
- a custom geometry DSL inside fenced ```geometry blocks (see `docs/GEOMETRY-DSL.md`)

YAML block-literal scalars (`|` or `|+`) preserve newlines verbatim — use them for any content with line breaks. Plain quoted strings work for one-liners.

---

## Worked end-to-end example

A lesson plus its dependencies, suitable for paste into another instance:

```yaml
tasks:
  - identifier: "1.1"
    title: "Einfache Gleichung"
    content: "Löse $2x = 10$."
    purpose: Practice
    primary:
      - { grid: "Lineare Algebra", description: "Gleichungen lösen", level: BasicLevel }
    secondary: []
    solutions:
      - { solutionType: Results, content: "$x = 5$" }
    attachments: []

assignments:
  - name: "Übung Lineare Gleichungen"
    description: "Hausübung zu linearen Gleichungen."
    assignmentDate: 2026-03-15
    activityType: HomeExercise
    groupSubmissionAllowed: false
    taskRefs: ["1.1"]

resources:
  - identifier: "Arbeitsblatt-Lineare-Gleichungen"
    content:
      tag: ExInlineContent
      contents: "Übungen zum Thema."
    competenceLevels: []
    attachments: []

lessons:
  - title: "Lineare Gleichungen einführen"
    description: "Einführung in das Lösen linearer Gleichungen."
    date: 2026-03-15
    competences: []
    phases:
      - title: "Einstieg"
        socialForm: WholeClass
        duration: 10
        actionForm: Presenting
        items:
          - { kind: ItemResource, ref: "Arbeitsblatt-Lineare-Gleichungen", publish: true }
      - title: "Erarbeitung"
        socialForm: IndividualWork
        duration: 20
        actionForm: Assigning
        items:
          - { kind: ItemAssignment, ref: "Übung Lineare Gleichungen", publish: true }
    supplementalItems: []
    notesTitleOverride: null
    assignmentRefs: ["Übung Lineare Gleichungen"]
```

---

## Tips for LLM use

- All field names and enum values are **English** and case-sensitive — never localise them. UI text shown to the user is German; format identifiers are not.
- Default to using YAML `|` block scalars for any rich-text field with newlines or special characters.
- An empty document `{}` is valid (all six lists default to empty); useful as a starting template.
- When generating from scratch, populate **only the lists you need** — omitted keys are equivalent to empty lists.
- The matcher is whitespace- and case-insensitive on grid titles and competence descriptions, so don't sweat exact whitespace there.
- The matcher is **case-sensitive** on task identifiers and resource identifiers — those are matched verbatim after a `.toLower . strip`, so consistent casing helps.
- If you need to introduce a new entity that doesn't exist on the receiving side yet, just include it in the appropriate list — the importer creates it.
- If you want to update an existing entity, use the same `name` / `identifier` / `title` and the importer will patch instead of duplicate.
- Lesson `phases[].items[].ref` and a lesson's `assignmentRefs` / `resourceRefs` are looked up against the freshly-applied entities, so referenced entities **must also appear** in the corresponding top-level list of the same import. (The export side handles this automatically; just make sure the LLM does too when generating from scratch.)

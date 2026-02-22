# Import & Export Reference

The application supports importing and exporting competence grids, tasks, assignments, resources, and lessons via a markdown-based text format. The import modal shows a preview of changes before applying them. Export copies the same format to the clipboard for external editing (e.g., with AI tools).

## General Concepts

**Round-trip**: Export produces the same format that the import parser accepts. You can export, edit externally, and re-import without losing data.

**Preview before apply**: The import modal shows the status of each entity:

- **Create** — new entity, will be added
- **Update** — existing entity found, will be modified
- **No Change** — existing entity found, content is identical

**Renaming**: To rename an entity during re-import, add `(Ersetzt: Original Name)` to the heading. The original name is used for matching; the new name replaces it. Example:

```
## Bruchrechnung (Ersetzt: Brüche)
```

This renames the competence "Brüche" to "Bruchrechnung". Omit the clause for new entities or unchanged re-imports.

**Markdown content**: Body sections (Angabe, Beschreibung, solutions) use the application's markdown syntax. See [MARKDOWN.md](MARKDOWN.md) for the full reference, including math, callouts, and formatting.

**Multiple entities**: Each import can contain multiple grids, tasks, assignments, resources, or lessons. Each top-level `#` heading starts a new entity.

## Competence Grid Format

### Example

```markdown
# Lineare Algebra

## Gleichungen lösen
- Wesentlich: Einfache Gleichungen mit einer Unbekannten lösen
- Mittelstufe: Gleichungssysteme mit zwei Unbekannten lösen
- Fortgeschritten: Gleichungssysteme mit Matrizenrechnung lösen

## Vektoren (Ersetzt: Vektorrechnung)
- Wesentlich: Vektoren addieren und subtrahieren
- Fortgeschritten: Kreuzprodukt berechnen und geometrisch interpretieren
```

### Format Rules

| Element | Syntax | Required |
|---|---|---|
| Grid title | `#` heading | Yes |
| Competence | `##` heading | At least one |
| Rename clause | `(Ersetzt: Original)` in `##` heading | No |
| Level description | `- Levelname: Description` | No |

**Level names** (case-sensitive, exactly as shown):

| Name | Meaning |
|---|---|
| `Wesentlich` | Basic level |
| `Mittelstufe` | Intermediate level |
| `Fortgeschritten` | Advanced level |

- Levels without a description can be omitted entirely.
- Competence ordering follows the order of `##` headings in the document.
- Multiple grids in one import — each `#` heading starts a new grid.

## Task Format

### Example

```markdown
# Buch-3.45 (Ersetzt: Buch-3.44)

## Angabe
Löse die Gleichung $2x + 5 = 17$ und gib die Lösungsmenge an.

## Kompetenzen
- Lineare Algebra / Gleichungen lösen / Wesentlich

## Hinweis
Bringe alle Terme mit $x$ auf eine Seite.

## Ergebnis
$\mathbb{L} = \{6\}$

## Komplettlösung
$$2x + 5 = 17$$
$$2x = 12$$
$$x = 6$$
Die Lösungsmenge ist $\mathbb{L} = \{6\}$.
```

### Format Rules

| Element | Syntax | Required |
|---|---|---|
| Task identifier | `#` heading (e.g., `Buch-3.45`) | Yes |
| Rename clause | `(Ersetzt: Original)` in `#` heading | No |
| Task content | `## Angabe` section | No |
| Competence references | `## Kompetenzen` section | No |
| Hint solution | `## Hinweis` section | No |
| Results solution | `## Ergebnis` section | No |
| Complete solution | `## Komplettlösung` section | No |

**Competence references** use `/` separators:

```
- Grid Title / Competence Description / Level
```

Each part is trimmed of surrounding whitespace. The level must be one of the three German level names (`Wesentlich`, `Mittelstufe`, `Fortgeschritten`).

- Sections can appear in any order.
- All sections except the `#` heading are optional.
- Multiple tasks in one import — each `#` heading starts a new task.

## Assignment Format

### Example

```markdown
# Mathematik-Test 3a (Ersetzt: Mathe-Test 3a)

## Beschreibung
Überprüfung zum Thema lineare Gleichungen und Vektorrechnung.

## Angaben
Date: 2026-02-15
Type: Prüfung

### Aufgabe-1

#### Angabe
Löse die Gleichung $3x - 7 = 2x + 5$.

#### Kompetenzen
- Lineare Algebra / Gleichungen lösen / Wesentlich

#### Ergebnis
$\mathbb{L} = \{12\}$

#### Komplettlösung
$$3x - 7 = 2x + 5$$
$$x = 12$$

### Aufgabe-2 (Ersetzt: Aufgabe-2-alt)

#### Angabe
Berechne das Kreuzprodukt der Vektoren $\vec{a} = (1, 2, 3)$ und $\vec{b} = (4, 5, 6)$.

#### Kompetenzen
- Lineare Algebra / Vektoren / Fortgeschritten

#### Hinweis
Verwende die Formel $\vec{a} \times \vec{b} = \begin{pmatrix} a_2 b_3 - a_3 b_2 \\ a_3 b_1 - a_1 b_3 \\ a_1 b_2 - a_2 b_1 \end{pmatrix}$.
```

### Format Rules

| Element | Syntax | Required |
|---|---|---|
| Assignment name | `#` heading | Yes |
| Rename clause | `(Ersetzt: Original)` in `#` heading | No |
| Description | `## Beschreibung` section | No |
| Metadata | `## Angaben` section | No |
| Embedded task | `###` heading (task identifier) | No |
| Task rename clause | `(Ersetzt: Original)` in `###` heading | No |
| Task sections | `####` headings (Angabe, Kompetenzen, etc.) | No |

Embedded tasks use the same section names as standalone tasks, but one heading level deeper (`####` instead of `##`).

**Metadata** in the `## Angaben` section uses `Key: Value` lines:

| Key | Format | Default |
|---|---|---|
| `Date` | `YYYY-MM-DD` (ISO date) | `2000-01-01` |
| `Type` | Activity type name | `Schulübung` |

### Activity Types

Both German (with or without umlauts) and English names are accepted during import. Export always uses the German form with umlauts.

| German | Without umlauts | English | Meaning |
|---|---|---|---|
| Gespräch | Gespraech | Conversation | Oral conversation |
| Prüfung | Pruefung | Exam | Written exam |
| Schulübung | Schuluebung | SchoolExercise | In-class exercise |
| Hausübung | Hausuebung | HomeExercise | Homework |

## Resource Format

### Example

```markdown
# Arbeitsblatt-7 (Ersetzt: Arbeitsblatt-7-alt)

## Inhalt
Übungsaufgaben zum Thema **Bruchrechnung**.

Bearbeite die Aufgaben 1–5 auf Seite 42.

## Kompetenzen
- Lineare Algebra / Gleichungen lösen / Wesentlich
- Lineare Algebra / Vektoren / Fortgeschritten
```

### Format Rules

| Element | Syntax | Required |
|---|---|---|
| Resource identifier | `#` heading | Yes |
| Rename clause | `(Ersetzt: Original)` in `#` heading | No |
| Inline content | `## Inhalt` section | No |
| Competence references | `## Kompetenzen` section | No |

**Competence references** use the same `/`-separated format as tasks: `Grid / Competence / Level`.

- All sections except the `#` heading are optional.
- Only inline content is supported. WebLink and VideoLink resources cannot be imported via this format.
- Multiple resources in one import — each `#` heading starts a new resource.

## Lesson Format

### Example

```markdown
# Lineare Gleichungen einführen (Ersetzt: Gleichungen Einführung)

## Angaben
Date: 2026-03-15

## Beschreibung
Einführung in das Lösen linearer Gleichungen mit einer Unbekannten.

## Kompetenzen
- Lineare Algebra / Gleichungen lösen / Wesentlich

## Materialien
- Arbeitsblatt-7
- Buch S.42

## Aufgaben
- Mathematik-Test 3a

## Phasen
- Einstieg / Plenum / Darbietend / 10 min
  Wiederholung der letzten Stunde.
- Erarbeitung / Gruppenarbeit / Zusammenwirkend / 20 min
  Schüler lösen Aufgaben in Kleingruppen.
- Input\/Output Phase / Einzelarbeit / Aufgebend / 15 min
  Selbstständiges Arbeiten am Arbeitsblatt.

## Notizen
Differenzierung: Leistungsstarke Schüler bearbeiten Zusatzaufgaben.
```

### Format Rules

| Element | Syntax | Required |
|---|---|---|
| Lesson title | `#` heading | Yes |
| Rename clause | `(Ersetzt: Original)` in `#` heading | No |
| Metadata | `## Angaben` section | No |
| Description | `## Beschreibung` section | No |
| Competence references | `## Kompetenzen` section | No |
| Resource references | `## Materialien` section | No |
| Assignment references | `## Aufgaben` section | No |
| Phases | `## Phasen` section | No |
| Notes | `## Notizen` section | No |

**Metadata** in the `## Angaben` section uses `Key: Value` lines:

| Key | Format | Default |
|---|---|---|
| `Date` | `YYYY-MM-DD` (ISO date) | None |

**Competence references** use the same `/`-separated format as tasks: `Grid / Competence / Level`.

**Resource and assignment references** are bullet lists of identifiers/names that reference existing entities. They are not created during lesson import — they must already exist in the document.

### Phase Format

Each phase is a bullet list item with exactly four `/`-separated parts:

```
- Title / SocialForm / ActionForm / Duration min
```

Optional indented notes can follow on subsequent lines:

```
- Einstieg / Plenum / Darbietend / 10 min
  Notes for this phase go here.
```

To include a literal `/` in the phase title, escape it as `\/`:

```
- Input\/Output Phase / Plenum / Darbietend / 10 min
```

**Social form names** (case-sensitive, exactly as shown):

| Name | Meaning |
|---|---|
| `Plenum` | Whole class |
| `Gruppenarbeit` | Small groups |
| `Partnerarbeit` | Pair work |
| `Einzelarbeit` | Individual work |

**Action form names** (case-sensitive, exactly as shown):

| Name | Meaning |
|---|---|
| `Darbietend` | Presenting |
| `Zusammenwirkend` | Collaborating |
| `Aufgebend` | Assigning |

- All sections except the `#` heading are optional.
- Lessons are imported into a specific MesoPlan, selected in the UI.
- Multiple lessons in one import — each `#` heading starts a new lesson.

## How to Use

1. **Import**: Open the import modal, paste the formatted text, click "Vorschau" to see a preview of changes, review the Create/Update/No Change status for each entity, then click "Anwenden" to apply.
2. **Export**: Click the export button on a competence grid, assignment, resource, or lesson. The formatted text is copied to the clipboard.
3. **AI editing workflow**: Export an entity, paste the text into an AI chat, describe the changes you want, then paste the AI's output back into the import modal.
4. **Lesson import**: Accessed from the Planning detail view. Select a MesoPlan first, then use the import modal to add lessons to it.

## Tips and Gotchas

- Level names must be exactly `Wesentlich`, `Mittelstufe`, or `Fortgeschritten` (case-sensitive).
- Competence references in tasks use `/` as separators: `Grid / Competence / Level`. All three parts are required for a match.
- The `(Ersetzt: ...)` clause is only needed when renaming. Omit it for new entities or unchanged re-imports.
- Date format is ISO: `YYYY-MM-DD`. Missing date defaults to `2000-01-01`, so always include it.
- Unrecognized activity types default to Schulübung (in-class exercise).
- Exported text uses German activity type names with umlauts (e.g., `Hausübung`). Both umlaut and non-umlaut forms are accepted on import.
- Empty sections (e.g., a solution with no content) are skipped during export and ignored during import.
- Phase format requires exactly four `/`-separated parts: `Title / SocialForm / ActionForm / Duration min`. Missing or extra parts cause the phase to be skipped.
- Social form names (`Plenum`, `Gruppenarbeit`, `Partnerarbeit`, `Einzelarbeit`) and action form names (`Darbietend`, `Zusammenwirkend`, `Aufgebend`) are case-sensitive.
- Use `\/` to escape literal slashes in phase titles (e.g., `Input\/Output`).
- Lesson import targets the currently selected MesoPlan. Make sure the correct plan is selected before importing.

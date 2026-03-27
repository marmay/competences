#!/usr/bin/env python3
"""Convert a competence grid markdown export to a LaTeX tabularx table.

Input format (as exported by the application's Export button):

    # Grid Title

    ## Ich kann lineare Gleichungen lösen
    - Wesentlich: Einfache Gleichungen mit einer Unbekannten
    - Mittelstufe: Gleichungssysteme mit zwei Unbekannten
    - Fortgeschritten: Textaufgaben in Gleichungen übersetzen

    ## Ich kann Brüche addieren
    - Wesentlich: Gleichnamige Brüche
    - Mittelstufe: Ungleichnamige Brüche

Output: LaTeX nested itemize list, one item per competence with (W)/(M)/(F) sub-items
"""

import argparse
import re
import sys

GRID_TITLE_RE = re.compile(r"^#\s+(.*)")
COMPETENCE_RE = re.compile(r"^##\s+(.*)")
LEVEL_RE = re.compile(r"^-\s+(Wesentlich|Mittelstufe|Fortgeschritten):\s+(.*)")

LEVEL_MAP = {
    "Wesentlich": "W",
    "Mittelstufe": "M",
    "Fortgeschritten": "F",
}

PREAMBLE = r"""\documentclass[a4paper]{{article}}
\usepackage[utf8]{{inputenc}}
\usepackage[T1]{{fontenc}}
\usepackage[ngerman]{{babel}}
\usepackage{{enumitem}}
\usepackage[margin=2cm]{{geometry}}

\begin{{document}}
"""

POSTAMBLE = r"""
\end{document}
"""


def parse(lines):
    """Parse exported markdown lines into a grid title and list of competences.

    Each competence is a dict with keys:
      - description: str
      - W, M, F: str or empty
    """
    title = None
    competences = []
    current = None

    for line in lines:
        line = line.rstrip("\n")

        if title is None:
            m = GRID_TITLE_RE.match(line)
            if m:
                title = m.group(1)
            continue

        m = COMPETENCE_RE.match(line)
        if m:
            current = {"description": m.group(1), "W": "", "M": "", "F": ""}
            competences.append(current)
            continue

        m = LEVEL_RE.match(line)
        if m and current is not None:
            level_key = LEVEL_MAP[m.group(1)]
            current[level_key] = m.group(2)

    return title, competences


def escape_latex(text):
    """Escape special LaTeX characters in text."""
    replacements = [
        ("\\", r"\textbackslash{}"),
        ("&", r"\&"),
        ("%", r"\%"),
        ("$", r"\$"),
        ("#", r"\#"),
        ("_", r"\_"),
        ("{", r"\{"),
        ("}", r"\}"),
        ("~", r"\textasciitilde{}"),
        ("^", r"\textasciicircum{}"),
    ]
    for old, new in replacements:
        text = text.replace(old, new)
    return text


def render_latex(title, competences, standalone=False, printLevels=True):
    """Render competences as a LaTeX nested itemize list."""
    parts = []

    if standalone:
        parts.append(PREAMBLE)

    if title:
        parts.append(r"\section*{" + escape_latex(title) + "}")
        parts.append("")

    parts.append(r"\begin{itemize}")

    for comp in competences:
        parts.append(r"  \item " + escape_latex(comp["description"]))
        if printLevels:
            levels = [(k, comp[k]) for k in ("W", "M", "F") if comp[k]]
            if levels:
                parts.append(r"  \begin{itemize}")
                for key, text in levels:
                    parts.append(r"    \item[(" + key + r")] " + escape_latex(text))
                    parts.append(r"  \end{itemize}")

    parts.append(r"\end{itemize}")

    if standalone:
        parts.append(POSTAMBLE)

    return "\n".join(parts)


def main():
    parser = argparse.ArgumentParser(
        description="Convert competence grid markdown export to LaTeX tabularx table."
    )
    parser.add_argument(
        "file",
        nargs="?",
        type=argparse.FileType("r", encoding="utf-8"),
        default=sys.stdin,
        help="Input file (default: stdin)",
    )
    parser.add_argument(
        "--standalone",
        action="store_true",
        help="Wrap table in a full LaTeX document",
    )
    parser.add_argument(
        "--nolevels",
        action="store_true",
        help="Do not print level descriptions",
    )
    args = parser.parse_args()

    lines = args.file.readlines()
    title, competences = parse(lines)

    if not competences:
        print("No competences found in input.", file=sys.stderr)
        sys.exit(1)

    print(render_latex(title, competences, standalone=args.standalone, printLevels = not(args.nolevels)))


if __name__ == "__main__":
    main()

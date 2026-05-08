#!/usr/bin/env python3
"""Convert a competence grid YAML export to a LaTeX tabularx table.

Input format (as exported by the application's Export button in YAML format):

    competenceGrids:
      - title: "Lineare Algebra"
        description: "Schemata für lineare Gleichungen und Vektorrechnung."
        competences:
          - description: "Gleichungen lösen"
            levels:
              BasicLevel: "Einfache Gleichungen mit einer Unbekannten lösen."
              IntermediateLevel: "Gleichungssysteme mit zwei Unbekannten lösen."
              AdvancedLevel: "Gleichungssysteme mit Matrizenrechnung lösen."

Output: LaTeX nested itemize list, one item per competence with (W)/(M)/(F) sub-items
"""

import argparse
import re
import sys

try:
    import yaml
except ImportError:
    print("Error: PyYAML is required. Install with: pip install pyyaml", file=sys.stderr)
    sys.exit(1)

# Mapping from YAML level names to short codes
YAML_LEVEL_MAP = {
    "BasicLevel": "W",
    "IntermediateLevel": "M",
    "AdvancedLevel": "F",
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


def parse_yaml(text):
    """Parse YAML export into grid title and list of competences.

    Each competence is a dict with keys:
      - description: str
      - W, M, F: str or empty
    """
    data = yaml.safe_load(text)
    
    if not data or 'competenceGrids' not in data:
        return None, []
    
    grids = data.get('competenceGrids', [])
    if not grids:
        return None, []
    
    # Take the first grid (for single-grid exports)
    grid = grids[0]
    title = grid.get('title', '')
    
    competences = []
    for comp in grid.get('competences', []):
        comp_dict = {
            "description": comp.get('description', ''),
            "W": "",
            "M": "",
            "F": ""
        }
        levels = comp.get('levels', {})
        
        # Handle two possible YAML representations of the levels map:
        # 1. As a dict: {BasicLevel: "text", IntermediateLevel: "text"}
        # 2. As a list of pairs: [[BasicLevel, "text"], [IntermediateLevel, "text"]]
        if isinstance(levels, dict):
            for yaml_level, short_code in YAML_LEVEL_MAP.items():
                if yaml_level in levels:
                    comp_dict[short_code] = levels[yaml_level]
        elif isinstance(levels, list):
            for pair in levels:
                if isinstance(pair, list) and len(pair) == 2:
                    yaml_level = pair[0]
                    description = pair[1]
                    if yaml_level in YAML_LEVEL_MAP:
                        comp_dict[YAML_LEVEL_MAP[yaml_level]] = description
        
        competences.append(comp_dict)
    
    return title, competences


def parse(lines):
    """Parse exported lines (YAML format) into a grid title and list of competences.

    Each competence is a dict with keys:
      - description: str
      - W, M, F: str or empty
    
    For backward compatibility, also accepts old markdown format.
    """
    text = ''.join(lines)
    
    # Try YAML format first
    title, competences = parse_yaml(text)
    if title or competences:
        return title, competences
    
    # Fallback to old markdown format for backward compatibility
    title = None
    competences = []
    current = None

    for line in lines:
        line = line.rstrip("\n")

        if title is None:
            m = re.compile(r"^#\s+(.*)").match(line)
            if m:
                title = m.group(1)
            continue

        m = re.compile(r"^##\s+(.*)").match(line)
        if m:
            current = {"description": m.group(1), "W": "", "M": "", "F": ""}
            competences.append(current)
            continue

        m = re.compile(r"^-\s+(Wesentlich|Mittelstufe|Fortgeschritten):\s+(.*)").match(line)
        if m and current is not None:
            level_key = {"Wesentlich": "W", "Mittelstufe": "M", "Fortgeschritten": "F"}[m.group(1)]
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
        description="Convert competence grid YAML export to LaTeX tabularx table."
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

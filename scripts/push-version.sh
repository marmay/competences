#!/bin/sh

CABAL_VERSION_FILES="
./backend/competences-backend.cabal
./housecup/competences-housecup.cabal
./frontend/competences-frontend.cabal
./common/competences-common.cabal
./markdown/competences-markdown.cabal
"

NIX_VERSION_FILES="
./nix/frontend.nix
"

FROM_VERSION="$1"
TO_VERSION="$2"

# For cabal files, append .0 to version numbers
CABAL_FROM_VERSION="${FROM_VERSION}.0"
CABAL_TO_VERSION="${TO_VERSION}.0"

# Validate files exist
for f in $CABAL_VERSION_FILES $NIX_VERSION_FILES; do
    if ! test -e "$f"; then
        echo "Required file does not exist: $f"
        exit 1
    fi
done

# Validate arguments
if test -z "$FROM_VERSION" -o -z "$TO_VERSION"; then
    echo "Usage: $0 FROM_VERSION TO_VERSION"
    echo "Please provide FROM and TO versions without .0 suffix; e.g. 1.6.2 1.7.0"
    exit 1
fi

# Bump cabal files (using .0 suffix)
for file in $CABAL_VERSION_FILES; do
    if ! sed -i.bak -e "s/^\(version:[[:space:]]*\)$CABAL_FROM_VERSION\$/\1$CABAL_TO_VERSION/" "$file"; then
        echo "Could not push version in $file!"
        exit 1
    fi
    rm -f "$file.bak"
done

# Bump nix files (without .0 suffix)
for file in $NIX_VERSION_FILES; do
    if ! sed -i.bak -e "s/^\([[:space:]]*version[[:space:]]*=[[:space:]]*\)\"$FROM_VERSION\";/\1\"$TO_VERSION\";/" "$file"; then
        echo "Could not push version in $file!"
        exit 1
    fi
    rm -f "$file.bak"
done

echo "Bumped version from $FROM_VERSION to $TO_VERSION in all files."

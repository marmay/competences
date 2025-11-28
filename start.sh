#!/bin/sh

set -e

CLASS=$1
if test -z "$CLASS"; then
    echo "Please provide the name of a class!";
    exit 1;
fi

echo "Looking for file for class \"$CLASS\"."

FILE=""
for candidate in "data/$CLASS.json" "$CLASS" "data/$CLASS"; do
    if test -f $candidate; then
	FILE=$candidate;
	break;
    fi
done

if ! test -f "$FILE"; then
    echo "Could not locate file for $CLASS!"
    exit 1;
fi

echo "Located file \"$FILE\"."
BACKUP_FILE="backups/$(basename $FILE .json)_$(date +%s).json"

echo "Creating backup of \"$FILE\" to \"$BACKUP_FILE\"."
mkdir -p backups
cp $FILE $BACKUP_FILE

echo "Starting frontend application; Press CTRL-C to abort."
cabal run exe:competences-frontend -- -u "5c0ae884-91fd-4e62-872d-75675db62fe8" -n "Markus Mayr" -r "Teacher" -i $FILE -o $FILE

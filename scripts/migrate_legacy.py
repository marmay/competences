#!/usr/bin/env python3
"""
Migrate legacy JSON documents to new format.

Usage: python migrate_legacy.py input.json output.json

Transformations:
- Add office365Id: "" to all users
- Add empty tasks, taskGroups, assignments arrays if missing
- Split evidences with multiple users into separate records
- Generate new UUIDs for split evidences and their observations
- Change userIds array to single userId field
- Keep evidences with no users (userId: null)
"""

import json
import sys
import uuid
from copy import deepcopy


def regenerate_observation_ids(observations):
    """Generate new UUIDs for all observations."""
    new_observations = []
    for obs in observations:
        new_obs = deepcopy(obs)
        new_obs["id"] = str(uuid.uuid4())
        new_observations.append(new_obs)
    return new_observations


def migrate_document(doc):
    # 1. Add office365Id to all users
    for user in doc.get("users", []):
        if "office365Id" not in user:
            user["office365Id"] = ""

    # 2. Add empty collection fields if missing
    if "tasks" not in doc:
        doc["tasks"] = []
    if "taskGroups" not in doc:
        doc["taskGroups"] = []
    if "assignments" not in doc:
        doc["assignments"] = []

    # 3, 4, 5. Split evidences by user and change to single userId
    new_evidences = []
    empty_count = 0
    split_count = 0

    for evidence in doc.get("evidences", []):
        user_ids = evidence.get("userIds", [])

        if len(user_ids) == 0:
            # No users - keep with null userId
            empty_count += 1
            new_evidence = deepcopy(evidence)
            del new_evidence["userIds"]
            new_evidence["userId"] = None
            new_evidences.append(new_evidence)
        elif len(user_ids) == 1:
            # Single user - just rename field, keep original IDs
            new_evidence = deepcopy(evidence)
            del new_evidence["userIds"]
            new_evidence["userId"] = user_ids[0]
            new_evidences.append(new_evidence)
        else:
            # Multiple users - split into separate evidences
            split_count += 1
            for i, user_id in enumerate(user_ids):
                new_evidence = deepcopy(evidence)
                del new_evidence["userIds"]
                new_evidence["userId"] = user_id

                if i == 0:
                    # First user keeps original evidence ID and observation IDs
                    pass
                else:
                    # Subsequent users get new IDs
                    new_evidence["id"] = str(uuid.uuid4())
                    new_evidence["observations"] = regenerate_observation_ids(
                        evidence.get("observations", [])
                    )

                new_evidences.append(new_evidence)

    doc["evidences"] = new_evidences

    return doc, empty_count, split_count


def main():
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} input.json output.json", file=sys.stderr)
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2]

    with open(input_path, "r", encoding="utf-8") as f:
        doc = json.load(f)

    migrated, empty, split = migrate_document(doc)

    with open(output_path, "w", encoding="utf-8") as f:
        json.dump(migrated, f, indent=2, ensure_ascii=False)

    print(f"Migrated: {input_path} -> {output_path}")
    print(f"  Users updated: {len(migrated.get('users', []))}")
    print(f"  Evidences: {len(migrated.get('evidences', []))} total")
    print(f"    Empty (no users): {empty}")
    print(f"    Split (multi-user): {split}")


if __name__ == "__main__":
    main()

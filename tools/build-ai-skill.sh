#!/usr/bin/env bash
# Assemble the Claude skill ai/studysim-scenario with the current format
# reference, prompt and example, and zip it into dist/ for upload.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
S="$ROOT/ai/studysim-scenario"
mkdir -p "$S/references" "$ROOT/dist"
cp "$ROOT/inst/library/schema.md" "$S/references/schema.md"
cp "$ROOT/ai/prompt-derive-to-scenario.md" "$S/references/prompt.md"
cp "$ROOT/inst/library/scenarios/breast_cancer_risk.yaml" "$S/references/example.yaml"
( cd "$ROOT/ai" && rm -f "$ROOT/dist/studysim-scenario-skill.zip" && zip -qr "$ROOT/dist/studysim-scenario-skill.zip" studysim-scenario )
echo "wrote dist/studysim-scenario-skill.zip"

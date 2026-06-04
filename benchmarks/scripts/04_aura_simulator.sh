#!/usr/bin/env bash
# Phase 3: Run Aura simulator experiments
# Produces all data needed for the NeurIPS paper
set -euo pipefail

RESULTS_DIR="$1"
PROJECT_DIR="$2"
AURA_RESULTS="$RESULTS_DIR/aura_simulator"
mkdir -p "$AURA_RESULTS"

cd "$PROJECT_DIR"

echo "Building Aura..."
sbt compile 2>&1 | tail -3

echo ""
echo "Running tests..."
sbt test 2>&1 | tail -5

# ── Experiment 1-7: Full Evaluation Suite ────────────────────────────────
echo ""
echo "--- Running InferenceEvaluationExample (Experiments 1-7) ---"
sbt "auraExamples/runMain io.aura.examples.InferenceEvaluationExample" 2>&1 \
  | tee "$AURA_RESULTS/evaluation_output.txt"

# Extract CSV blocks from output
python3 -c "
import sys

with open('$AURA_RESULTS/evaluation_output.txt') as f:
    lines = f.readlines()

current_csv = None
csv_lines = []

for line in lines:
    line = line.strip()
    if line.startswith('[CSV:'):
        if current_csv and csv_lines:
            with open(f'$AURA_RESULTS/{current_csv}.csv', 'w') as out:
                out.write('\n'.join(csv_lines) + '\n')
        current_csv = line[5:-1]  # extract name between [CSV: and ]
        csv_lines = []
    elif current_csv and ',' in line and not line.startswith('=') and not line.startswith('-'):
        csv_lines.append(line)

if current_csv and csv_lines:
    with open(f'$AURA_RESULTS/{current_csv}.csv', 'w') as out:
        out.write('\n'.join(csv_lines) + '\n')

print(f'Extracted CSV files from evaluation output')
"

# ── Paper Figures Data ───────────────────────────────────────────────────
echo ""
echo "--- Running InferencePaperFigures (8 figure datasets) ---"
sbt "auraExamples/runMain io.aura.examples.InferencePaperFigures" 2>&1 \
  | tee "$AURA_RESULTS/figures_output.txt"

# Extract figure CSVs
python3 -c "
with open('$AURA_RESULTS/figures_output.txt') as f:
    lines = f.readlines()

current_csv = None
csv_lines = []

for line in lines:
    line = line.strip()
    if line.startswith('[CSV:'):
        if current_csv and csv_lines:
            with open(f'$AURA_RESULTS/{current_csv}.csv', 'w') as out:
                out.write('\n'.join(csv_lines) + '\n')
        current_csv = line[5:-1]
        csv_lines = []
    elif current_csv and ',' in line and not line.startswith('=') and not line.startswith('-'):
        csv_lines.append(line)

if current_csv and csv_lines:
    with open(f'$AURA_RESULTS/{current_csv}.csv', 'w') as out:
        out.write('\n'.join(csv_lines) + '\n')

print(f'Extracted figure CSV files')
"

# ── Scale Benchmark ──────────────────────────────────────────────────────
echo ""
echo "--- Running InferenceScaleBenchmark (1K-1M requests) ---"
sbt "auraExamples/runMain io.aura.examples.InferenceScaleBenchmark" 2>&1 \
  | tee "$AURA_RESULTS/scale_output.txt"

# Extract scale CSV
python3 -c "
with open('$AURA_RESULTS/scale_output.txt') as f:
    lines = f.readlines()

current_csv = None
csv_lines = []

for line in lines:
    line = line.strip()
    if line.startswith('[CSV:'):
        if current_csv and csv_lines:
            with open(f'$AURA_RESULTS/{current_csv}.csv', 'w') as out:
                out.write('\n'.join(csv_lines) + '\n')
        current_csv = line[5:-1]
        csv_lines = []
    elif current_csv and ',' in line and not line.startswith('=') and not line.startswith('-'):
        csv_lines.append(line)

if current_csv and csv_lines:
    with open(f'$AURA_RESULTS/{current_csv}.csv', 'w') as out:
        out.write('\n'.join(csv_lines) + '\n')

print(f'Extracted scale benchmark CSV')
"

# ── Parallelism Comparison ───────────────────────────────────────────────
echo ""
echo "--- Running InferenceParallelismExample ---"
sbt "auraExamples/runMain io.aura.examples.InferenceParallelismExample" 2>&1 \
  | tee "$AURA_RESULTS/parallelism_output.txt"

# ── CAGR Example ────────────────────────────────────────────────────────
echo ""
echo "--- Running InferenceCagrExample ---"
sbt "auraExamples/runMain io.aura.examples.InferenceCagrExample" 2>&1 \
  | tee "$AURA_RESULTS/cagr_output.txt"

echo ""
echo "Phase 3 complete. Aura results in $AURA_RESULTS/"
echo "CSV files generated:"
ls -la "$AURA_RESULTS"/*.csv 2>/dev/null || echo "  (no CSV files found)"

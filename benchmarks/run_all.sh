#!/usr/bin/env bash
# Aura-Inference NeurIPS 2026 — Full Benchmark Suite
# Run this on an Azure GPU VM with NVIDIA A100/H100
#
# Usage:
#   chmod +x benchmarks/run_all.sh
#   ./benchmarks/run_all.sh [--skip-vllm] [--skip-aura] [--skip-dvfs]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
RESULTS_DIR="$SCRIPT_DIR/results/$(date +%Y%m%d_%H%M%S)"
mkdir -p "$RESULTS_DIR"

SKIP_VLLM=false
SKIP_AURA=false
SKIP_DVFS=false

for arg in "$@"; do
  case $arg in
    --skip-vllm) SKIP_VLLM=true ;;
    --skip-aura) SKIP_AURA=true ;;
    --skip-dvfs) SKIP_DVFS=true ;;
  esac
done

echo "========================================================================"
echo "Aura-Inference NeurIPS 2026 Benchmark Suite"
echo "Results directory: $RESULTS_DIR"
echo "========================================================================"

# Detect GPU
GPU_NAME=$(nvidia-smi --query-gpu=name --format=csv,noheader | head -1 | xargs)
GPU_COUNT=$(nvidia-smi --query-gpu=name --format=csv,noheader | wc -l | xargs)
echo "Detected: ${GPU_COUNT}x ${GPU_NAME}"
echo "$GPU_NAME" > "$RESULTS_DIR/gpu_info.txt"
nvidia-smi >> "$RESULTS_DIR/gpu_info.txt"

# ── Phase 1: Real vLLM Inference Benchmarks ─────────────────────────────
if [ "$SKIP_VLLM" = false ]; then
  echo ""
  echo "════════════════════════════════════════════════════════════════"
  echo "Phase 1: Real vLLM Inference Benchmarks"
  echo "════════════════════════════════════════════════════════════════"
  bash "$SCRIPT_DIR/scripts/01_vllm_latency.sh" "$RESULTS_DIR"
  bash "$SCRIPT_DIR/scripts/02_power_monitor.sh" "$RESULTS_DIR"
else
  echo "Skipping vLLM benchmarks (--skip-vllm)"
fi

# ── Phase 2: PA-DVFS Real Hardware Experiment ────────────────────────────
if [ "$SKIP_DVFS" = false ]; then
  echo ""
  echo "════════════════════════════════════════════════════════════════"
  echo "Phase 2: PA-DVFS Real Hardware Experiment"
  echo "════════════════════════════════════════════════════════════════"
  bash "$SCRIPT_DIR/scripts/03_dvfs_experiment.sh" "$RESULTS_DIR"
else
  echo "Skipping DVFS experiment (--skip-dvfs)"
fi

# ── Phase 3: Aura Simulator Runs ────────────────────────────────────────
if [ "$SKIP_AURA" = false ]; then
  echo ""
  echo "════════════════════════════════════════════════════════════════"
  echo "Phase 3: Aura Simulator Runs"
  echo "════════════════════════════════════════════════════════════════"
  bash "$SCRIPT_DIR/scripts/04_aura_simulator.sh" "$RESULTS_DIR" "$PROJECT_DIR"
else
  echo "Skipping Aura simulator (--skip-aura)"
fi

# ── Phase 4: Comparison & Analysis ──────────────────────────────────────
echo ""
echo "════════════════════════════════════════════════════════════════"
echo "Phase 4: Comparison & Analysis"
echo "════════════════════════════════════════════════════════════════"
python3 "$SCRIPT_DIR/scripts/05_compare_results.py" "$RESULTS_DIR"

echo ""
echo "========================================================================"
echo "All benchmarks complete. Results in: $RESULTS_DIR"
echo "========================================================================"
ls -la "$RESULTS_DIR"

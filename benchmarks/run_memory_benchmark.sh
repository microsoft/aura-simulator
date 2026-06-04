#!/usr/bin/env bash
# Re-run IaaS memory benchmarks for the IEEE CLOUD 2026 short paper.
#
# Captures two memory metrics per scenario:
#   1. Peak heap usage via JMX MemoryPoolMXBean (in-process, true heap peak)
#   2. Peak RSS via /usr/bin/time -v (process-level resident set size)
#
# Run on the same Azure Standard_F8s_v2 VM as the original benchmarks
# to keep the platform claim in the paper consistent.
#
# Usage:
#   ./benchmarks/run_memory_benchmark.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
AURA_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
CS_DIR="$AURA_DIR/cloudsim-comparison"
RESULTS_DIR="$SCRIPT_DIR/results/memory_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$RESULTS_DIR"

# Verify required tools
command -v /usr/bin/time >/dev/null 2>&1 || {
  echo "ERROR: GNU time required at /usr/bin/time. Install via: sudo apt install time"
  exit 1
}
command -v sbt >/dev/null 2>&1 || { echo "ERROR: sbt not found"; exit 1; }

echo "============================================================"
echo "IaaS Memory Benchmark Re-run"
echo "Results: $RESULTS_DIR"
echo "Platform: $(uname -srm)"
echo "JVM: $(java -version 2>&1 | head -1)"
echo "============================================================"

# ── Aura: JMX peak heap + RSS via /usr/bin/time -v ──────────────────────
echo ""
echo "─── Aura PaperMemoryBenchmark (peak heap + RSS) ─────────────────"
cd "$AURA_DIR"

# Pre-stage with a no-op compile to avoid sbt boot noise in time -v output
sbt "auraBench/compile" >/dev/null

# Run under /usr/bin/time -v; sbt will fork JVM internally for runMain
/usr/bin/time -v \
  sbt "auraBench/runMain io.aura.bench.PaperMemoryBenchmark" \
  2> "$RESULTS_DIR/aura_time.log" \
  | tee "$RESULTS_DIR/aura_stdout.log"

echo ""
echo "Aura peak RSS:"
grep "Maximum resident set size" "$RESULTS_DIR/aura_time.log" || true

# ── CloudSim Plus: JMX peak heap + RSS ──────────────────────────────────
echo ""
echo "─── CloudSim Plus benchmark (peak heap + RSS) ───────────────────"
cd "$CS_DIR"

# Build fat jar so /usr/bin/time wraps the JVM directly (no gradle daemon noise)
gradle --no-daemon fatJar >/dev/null

CS_JAR=$(ls build/libs/cloudsim-comparison-*-all.jar | head -1)
echo "Using jar: $CS_JAR"

/usr/bin/time -v \
  java -Xmx8g -jar "$CS_JAR" \
  2> "$RESULTS_DIR/cs_time.log" \
  | tee "$RESULTS_DIR/cs_stdout.log"

echo ""
echo "CloudSim Plus peak RSS:"
grep "Maximum resident set size" "$RESULTS_DIR/cs_time.log" || true

echo ""
echo "============================================================"
echo "Done. Results in: $RESULTS_DIR"
echo ""
echo "Final numbers to plug into paper Table II:"
echo "  - Aura peak heap (JMX):  $RESULTS_DIR/aura_stdout.log (look for 'Peak heap' lines)"
echo "  - Aura peak RSS:         $RESULTS_DIR/aura_time.log (Maximum resident set size)"
echo "  - CS+ peak heap (JMX):   $RESULTS_DIR/cs_stdout.log (CSV column 4)"
echo "  - CS+ peak RSS:          $RESULTS_DIR/cs_time.log (Maximum resident set size)"
echo "============================================================"

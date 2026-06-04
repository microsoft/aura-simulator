#!/usr/bin/env bash
# Phase 2: PA-DVFS Real Hardware Experiment
# Tests GPU frequency scaling during decode-heavy workloads
# Requires root/sudo for nvidia-smi frequency locking
set -euo pipefail

RESULTS_DIR="$1"
DVFS_RESULTS="$RESULTS_DIR/dvfs_experiment"
mkdir -p "$DVFS_RESULTS"

MODEL="meta-llama/Llama-2-7b-hf"
PORT=8000
PROMPT_TOKENS=128
OUTPUT_TOKENS=256  # Long decode to emphasize memory-bound phase
BATCH_SIZE=32
RUNS=3

# Get GPU frequency range
GPU_MAX_FREQ=$(nvidia-smi --query-gpu=clocks.max.graphics --format=csv,noheader,nounits | head -1 | xargs)
GPU_MIN_FREQ=$(nvidia-smi --query-gpu=clocks.min.graphics --format=csv,noheader,nounits 2>/dev/null | head -1 | xargs || echo "210")

echo "GPU frequency range: ${GPU_MIN_FREQ} - ${GPU_MAX_FREQ} MHz"

# Frequency levels to test (simulating PA-DVFS decode frequencies)
# 100%, 80%, 60%, 40% of range
FREQ_RANGE=$((GPU_MAX_FREQ - GPU_MIN_FREQ))
FREQ_100=$GPU_MAX_FREQ
FREQ_80=$((GPU_MIN_FREQ + FREQ_RANGE * 80 / 100))
FREQ_60=$((GPU_MIN_FREQ + FREQ_RANGE * 60 / 100))
FREQ_40=$((GPU_MIN_FREQ + FREQ_RANGE * 40 / 100))

FREQUENCIES=($FREQ_100 $FREQ_80 $FREQ_60 $FREQ_40)
FREQ_LABELS=("100pct" "80pct" "60pct" "40pct")

echo "Test frequencies: ${FREQUENCIES[*]} MHz"

# Enable persistence mode for frequency locking
sudo nvidia-smi -pm 1 2>/dev/null || echo "Warning: Could not enable persistence mode"

# Start vLLM server
echo "Starting vLLM server..."
python3 -m vllm.entrypoints.openai.api_server \
  --model "$MODEL" \
  --port $PORT \
  --disable-log-requests \
  --max-model-len 4096 \
  > "$DVFS_RESULTS/server.log" 2>&1 &
VLLM_PID=$!

for i in $(seq 1 120); do
  if curl -s "http://localhost:$PORT/health" > /dev/null 2>&1; then
    echo "Server ready"
    break
  fi
  sleep 1
done

PROMPT=$(python3 -c "print('Explain the following concept in detail: ' + 'quantum computing ' * $((PROMPT_TOKENS / 3)))")

cat > "$DVFS_RESULTS/dvfs_bench.py" << 'PYEOF'
import sys
import time
import subprocess
import concurrent.futures
import requests
import json

results_dir = sys.argv[1]
prompt = sys.argv[2]
output_tokens = int(sys.argv[3])
batch_size = int(sys.argv[4])
runs = int(sys.argv[5])
freq_mhz = int(sys.argv[6])
freq_label = sys.argv[7]

url = "http://localhost:8000/v1/completions"

print(f"\n=== Frequency: {freq_mhz} MHz ({freq_label}) ===")

# Lock GPU frequency
subprocess.run(
    ["sudo", "nvidia-smi", "-lgc", f"{freq_mhz},{freq_mhz}"],
    capture_output=True
)
time.sleep(2)  # let frequency stabilize

all_results = []

for run in range(runs):
    # Start power monitoring
    power_log = f"{results_dir}/{freq_label}_run{run}_power.csv"
    monitor = subprocess.Popen(
        ["nvidia-smi", "dmon", "-s", "puc", "-d", "1"],
        stdout=open(power_log, 'w'),
        stderr=subprocess.DEVNULL
    )
    time.sleep(1)

    def send_request():
        start = time.perf_counter()
        first_token = None
        tokens = 0

        resp = requests.post(url, json={
            "model": "meta-llama/Llama-2-7b-hf",
            "prompt": prompt,
            "max_tokens": output_tokens,
            "temperature": 0,
            "stream": True
        }, stream=True)

        for line in resp.iter_lines():
            if line:
                s = line.decode('utf-8')
                if s.startswith('data: ') and s != 'data: [DONE]':
                    tokens += 1
                    if first_token is None:
                        first_token = time.perf_counter()

        end = time.perf_counter()
        if first_token is None:
            first_token = end

        return {
            'ttft': (first_token - start) * 1000,
            'tpot': (end - first_token) * 1000 / max(tokens - 1, 1),
            'total': (end - start) * 1000,
            'tokens': tokens
        }

    start_all = time.perf_counter()
    with concurrent.futures.ThreadPoolExecutor(max_workers=batch_size) as ex:
        futures = [ex.submit(send_request) for _ in range(batch_size)]
        results = [f.result() for f in concurrent.futures.as_completed(futures)]
    elapsed = time.perf_counter() - start_all

    time.sleep(1)
    monitor.terminate()
    monitor.wait()

    # Parse power
    powers = []
    with open(power_log) as f:
        for line in f:
            line = line.strip()
            if line.startswith('#') or not line:
                continue
            parts = line.split()
            if len(parts) >= 2:
                try:
                    powers.append(float(parts[1]))
                except ValueError:
                    pass

    avg_ttft = sum(r['ttft'] for r in results) / len(results)
    avg_tpot = sum(r['tpot'] for r in results) / len(results)
    avg_power = sum(powers) / len(powers) if powers else 0
    energy_j = avg_power * elapsed
    tokens_total = sum(r['tokens'] for r in results)
    energy_per_token = energy_j / max(tokens_total, 1) * 1000  # mJ/token

    row = {
        'freq_mhz': freq_mhz,
        'freq_label': freq_label,
        'run': run,
        'batch_size': batch_size,
        'avg_ttft_ms': avg_ttft,
        'avg_tpot_ms': avg_tpot,
        'avg_power_w': avg_power,
        'energy_j': energy_j,
        'energy_per_token_mj': energy_per_token,
        'total_tokens': tokens_total,
        'elapsed_s': elapsed,
    }
    all_results.append(row)

    print(f"  Run {run}: TTFT={avg_ttft:.1f}ms  TPOT={avg_tpot:.2f}ms  "
          f"Power={avg_power:.0f}W  Energy/tok={energy_per_token:.2f}mJ")

    time.sleep(2)

# Write CSV
with open(f"{results_dir}/dvfs_{freq_label}.csv", 'w') as f:
    f.write("freq_mhz,freq_label,run,batch_size,avg_ttft_ms,avg_tpot_ms,"
            "avg_power_w,energy_j,energy_per_token_mj,total_tokens,elapsed_s\n")
    for r in all_results:
        f.write(f"{r['freq_mhz']},{r['freq_label']},{r['run']},{r['batch_size']},"
                f"{r['avg_ttft_ms']:.2f},{r['avg_tpot_ms']:.2f},"
                f"{r['avg_power_w']:.1f},{r['energy_j']:.2f},"
                f"{r['energy_per_token_mj']:.2f},{r['total_tokens']},"
                f"{r['elapsed_s']:.2f}\n")
PYEOF

# Run at each frequency
for i in "${!FREQUENCIES[@]}"; do
  python3 "$DVFS_RESULTS/dvfs_bench.py" \
    "$DVFS_RESULTS" "$PROMPT" "$OUTPUT_TOKENS" "$BATCH_SIZE" "$RUNS" \
    "${FREQUENCIES[$i]}" "${FREQ_LABELS[$i]}" \
    | tee -a "$DVFS_RESULTS/dvfs_output.txt"
done

# Reset GPU frequency to default
sudo nvidia-smi -rgc 2>/dev/null || echo "Warning: Could not reset GPU clocks"

# Merge all DVFS CSVs
echo ""
echo "Merging DVFS results..."
echo "freq_mhz,freq_label,run,batch_size,avg_ttft_ms,avg_tpot_ms,avg_power_w,energy_j,energy_per_token_mj,total_tokens,elapsed_s" \
  > "$DVFS_RESULTS/dvfs_all.csv"
for label in "${FREQ_LABELS[@]}"; do
  tail -n +2 "$DVFS_RESULTS/dvfs_${label}.csv" >> "$DVFS_RESULTS/dvfs_all.csv"
done

# Cleanup
kill $VLLM_PID 2>/dev/null || true
wait $VLLM_PID 2>/dev/null || true

echo ""
echo "Phase 2 complete: DVFS results in $DVFS_RESULTS/dvfs_all.csv"
echo ""
echo "Summary:"
python3 -c "
import csv
with open('$DVFS_RESULTS/dvfs_all.csv') as f:
    reader = csv.DictReader(f)
    rows = list(reader)

# Average by frequency
from collections import defaultdict
by_freq = defaultdict(list)
for r in rows:
    by_freq[r['freq_label']].append(r)

baseline_energy = None
print(f\"{'Freq':>8s} {'TPOT(ms)':>10s} {'Power(W)':>10s} {'mJ/tok':>10s} {'E savings':>10s} {'TPOT impact':>12s}\")
print('-' * 65)
for label in ['100pct', '80pct', '60pct', '40pct']:
    if label not in by_freq:
        continue
    group = by_freq[label]
    avg_tpot = sum(float(r['avg_tpot_ms']) for r in group) / len(group)
    avg_power = sum(float(r['avg_power_w']) for r in group) / len(group)
    avg_eptoken = sum(float(r['energy_per_token_mj']) for r in group) / len(group)
    if baseline_energy is None:
        baseline_energy = avg_eptoken
        baseline_tpot = avg_tpot
    e_save = (1 - avg_eptoken / baseline_energy) * 100
    t_impact = (avg_tpot / baseline_tpot - 1) * 100
    print(f'{label:>8s} {avg_tpot:>10.2f} {avg_power:>10.0f} {avg_eptoken:>10.2f} {e_save:>9.1f}% {t_impact:>11.1f}%')
"

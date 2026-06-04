#!/usr/bin/env bash
# Phase 1b: GPU Power Monitoring during inference
# Captures nvidia-smi power readings during prefill-heavy and decode-heavy workloads
set -euo pipefail

RESULTS_DIR="$1"
POWER_RESULTS="$RESULTS_DIR/power_monitor"
mkdir -p "$POWER_RESULTS"

MODEL="meta-llama/Llama-2-7b-hf"
PORT=8000

echo "Starting vLLM server for power measurement..."
python3 -m vllm.entrypoints.openai.api_server \
  --model "$MODEL" \
  --port $PORT \
  --disable-log-requests \
  --max-model-len 4096 \
  > "$POWER_RESULTS/server.log" 2>&1 &
VLLM_PID=$!

# Wait for server
for i in $(seq 1 120); do
  if curl -s "http://localhost:$PORT/health" > /dev/null 2>&1; then
    echo "Server ready after ${i}s"
    break
  fi
  sleep 1
done

# Record idle power (5 seconds)
echo "Recording idle power (5s)..."
nvidia-smi dmon -s p -d 1 -c 5 > "$POWER_RESULTS/idle_power.txt" 2>&1

cat > "$POWER_RESULTS/power_workload.py" << 'PYEOF'
import sys
import time
import subprocess
import threading
import requests
import json

results_dir = sys.argv[1]
url = "http://localhost:8000/v1/completions"

scenarios = [
    # (name, prompt_tokens_approx, output_tokens, batch_size, description)
    ("prefill_heavy_bs1",   2048, 8,   1,  "Long prefill, minimal decode, bs=1"),
    ("prefill_heavy_bs32",  2048, 8,   32, "Long prefill, minimal decode, bs=32"),
    ("decode_heavy_bs1",    32,   256, 1,  "Short prefill, long decode, bs=1"),
    ("decode_heavy_bs32",   32,   256, 32, "Short prefill, long decode, bs=32"),
    ("balanced_bs1",        512,  128, 1,  "Balanced, bs=1"),
    ("balanced_bs32",       512,  128, 32, "Balanced, bs=32"),
]

all_power_data = []

for name, prompt_len, output_len, bs, desc in scenarios:
    print(f"\n--- {name}: {desc} ---")

    prompt = "Explain the theory of " + "relativity " * (prompt_len // 2)

    # Start power monitoring
    power_log = f"{results_dir}/{name}_power.csv"
    monitor_proc = subprocess.Popen(
        ["nvidia-smi", "dmon", "-s", "puc", "-d", "1"],
        stdout=open(power_log, 'w'),
        stderr=subprocess.DEVNULL
    )

    time.sleep(1)  # let monitor start

    # Send requests
    import concurrent.futures

    def send():
        resp = requests.post(url, json={
            "model": "meta-llama/Llama-2-7b-hf",
            "prompt": prompt,
            "max_tokens": output_len,
            "temperature": 0,
        })
        return resp.json()

    start = time.perf_counter()
    with concurrent.futures.ThreadPoolExecutor(max_workers=bs) as ex:
        futures = [ex.submit(send) for _ in range(bs)]
        results = [f.result() for f in concurrent.futures.as_completed(futures)]
    elapsed = time.perf_counter() - start

    time.sleep(1)  # capture tail power

    # Stop monitor
    monitor_proc.terminate()
    monitor_proc.wait()

    # Parse power readings
    powers = []
    with open(power_log) as f:
        for line in f:
            line = line.strip()
            if line.startswith('#') or not line:
                continue
            parts = line.split()
            if len(parts) >= 2:
                try:
                    power_w = float(parts[1])
                    powers.append(power_w)
                except ValueError:
                    pass

    if powers:
        avg_power = sum(powers) / len(powers)
        peak_power = max(powers)
        min_power = min(powers)
    else:
        avg_power = peak_power = min_power = 0

    print(f"  Duration: {elapsed:.1f}s | Avg power: {avg_power:.0f}W | "
          f"Peak: {peak_power:.0f}W | Min: {min_power:.0f}W | Samples: {len(powers)}")

    all_power_data.append({
        'scenario': name,
        'batch_size': bs,
        'prompt_tokens': prompt_len,
        'output_tokens': output_len,
        'duration_s': elapsed,
        'avg_power_w': avg_power,
        'peak_power_w': peak_power,
        'min_power_w': min_power,
        'samples': len(powers),
    })

# Write summary CSV
with open(f"{results_dir}/power_summary.csv", 'w') as f:
    f.write("scenario,batch_size,prompt_tokens,output_tokens,duration_s,"
            "avg_power_w,peak_power_w,min_power_w,samples\n")
    for d in all_power_data:
        f.write(f"{d['scenario']},{d['batch_size']},{d['prompt_tokens']},"
                f"{d['output_tokens']},{d['duration_s']:.2f},"
                f"{d['avg_power_w']:.1f},{d['peak_power_w']:.1f},"
                f"{d['min_power_w']:.1f},{d['samples']}\n")

print(f"\nPower summary written to {results_dir}/power_summary.csv")
PYEOF

echo ""
echo "Running power measurement scenarios..."
python3 "$POWER_RESULTS/power_workload.py" "$POWER_RESULTS" \
  | tee "$POWER_RESULTS/power_output.txt"

# Cleanup
kill $VLLM_PID 2>/dev/null || true
wait $VLLM_PID 2>/dev/null || true

echo "Phase 1b complete: Power data in $POWER_RESULTS/"

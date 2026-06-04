#!/usr/bin/env bash
# Phase 1: Benchmark real vLLM inference latency at varying batch sizes
# Measures TTFT and TPOT for LLaMA-2-7B on the available GPU
set -euo pipefail

RESULTS_DIR="$1"
VLLM_RESULTS="$RESULTS_DIR/vllm_latency"
mkdir -p "$VLLM_RESULTS"

MODEL="meta-llama/Llama-2-7b-hf"
PROMPT_TOKENS=512
OUTPUT_TOKENS=128
PORT=8000
RUNS_PER_CONFIG=3

echo "Installing vLLM if needed..."
pip install -q vllm openai 2>/dev/null || pip install vllm openai

# Start vLLM server in background
echo "Starting vLLM server with $MODEL..."
python3 -m vllm.entrypoints.openai.api_server \
  --model "$MODEL" \
  --port $PORT \
  --disable-log-requests \
  --max-model-len 4096 \
  > "$VLLM_RESULTS/server.log" 2>&1 &
VLLM_PID=$!

# Wait for server to be ready
echo "Waiting for vLLM server to start..."
for i in $(seq 1 120); do
  if curl -s "http://localhost:$PORT/health" > /dev/null 2>&1; then
    echo "vLLM server ready after ${i}s"
    break
  fi
  if [ $i -eq 120 ]; then
    echo "ERROR: vLLM server failed to start"
    kill $VLLM_PID 2>/dev/null || true
    exit 1
  fi
  sleep 1
done

# Generate a fixed prompt of ~PROMPT_TOKENS tokens
PROMPT=$(python3 -c "print('The quick brown fox ' * $((PROMPT_TOKENS / 5)))")

# Write the benchmark script
cat > "$VLLM_RESULTS/bench_client.py" << 'PYEOF'
import sys
import json
import time
import requests

results_dir = sys.argv[1]
prompt = sys.argv[2]
output_tokens = int(sys.argv[3])
runs = int(sys.argv[4])

batch_sizes = [1, 2, 4, 8, 16, 32, 64, 128, 256]
url = "http://localhost:8000/v1/completions"

print("batch_size,run,ttft_ms,tpot_ms,total_time_ms,output_tokens")

all_results = []

for bs in batch_sizes:
    for run in range(runs):
        # Send bs concurrent requests using threading
        import concurrent.futures

        results_list = []

        def send_request():
            start = time.perf_counter()
            first_token_time = None
            tokens_received = 0

            # Use streaming to measure TTFT
            resp = requests.post(url, json={
                "model": "meta-llama/Llama-2-7b-hf",
                "prompt": prompt,
                "max_tokens": output_tokens,
                "temperature": 0,
                "stream": True
            }, stream=True)

            for line in resp.iter_lines():
                if line:
                    line_str = line.decode('utf-8')
                    if line_str.startswith('data: ') and line_str != 'data: [DONE]':
                        tokens_received += 1
                        if first_token_time is None:
                            first_token_time = time.perf_counter()

            end = time.perf_counter()

            if first_token_time is None:
                first_token_time = end

            ttft = (first_token_time - start) * 1000
            total = (end - start) * 1000
            tpot = (end - first_token_time) * 1000 / max(tokens_received - 1, 1)

            return {
                'ttft_ms': ttft,
                'tpot_ms': tpot,
                'total_ms': total,
                'tokens': tokens_received
            }

        with concurrent.futures.ThreadPoolExecutor(max_workers=bs) as executor:
            futures = [executor.submit(send_request) for _ in range(bs)]
            results_list = [f.result() for f in concurrent.futures.as_completed(futures)]

        # Report average across the batch
        avg_ttft = sum(r['ttft_ms'] for r in results_list) / len(results_list)
        avg_tpot = sum(r['tpot_ms'] for r in results_list) / len(results_list)
        avg_total = sum(r['total_ms'] for r in results_list) / len(results_list)
        avg_tokens = sum(r['tokens'] for r in results_list) / len(results_list)

        row = f"{bs},{run},{avg_ttft:.2f},{avg_tpot:.2f},{avg_total:.2f},{avg_tokens:.0f}"
        print(row)
        all_results.append(row)

        # Brief pause between runs
        time.sleep(2)

# Write CSV
with open(f"{results_dir}/vllm_latency.csv", 'w') as f:
    f.write("batch_size,run,ttft_ms,tpot_ms,total_time_ms,output_tokens\n")
    for row in all_results:
        f.write(row + '\n')

print(f"\nResults written to {results_dir}/vllm_latency.csv")
PYEOF

echo ""
echo "Running vLLM latency benchmarks..."
echo "  Model: $MODEL"
echo "  Prompt tokens: ~$PROMPT_TOKENS"
echo "  Output tokens: $OUTPUT_TOKENS"
echo "  Runs per config: $RUNS_PER_CONFIG"
echo "  Batch sizes: 1, 2, 4, 8, 16, 32, 64, 128, 256"
echo ""

python3 "$VLLM_RESULTS/bench_client.py" "$VLLM_RESULTS" "$PROMPT" "$OUTPUT_TOKENS" "$RUNS_PER_CONFIG" \
  | tee "$VLLM_RESULTS/bench_output.txt"

# Cleanup
echo "Stopping vLLM server..."
kill $VLLM_PID 2>/dev/null || true
wait $VLLM_PID 2>/dev/null || true

echo "Phase 1 complete: vLLM latency results in $VLLM_RESULTS/"

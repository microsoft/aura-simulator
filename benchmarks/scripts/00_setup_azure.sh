#!/usr/bin/env bash
# Phase 0: Azure GPU VM Setup
# Run this first on a fresh Azure NC/ND-series VM
set -euo pipefail

echo "========================================================================"
echo "Aura-Inference Azure VM Setup"
echo "========================================================================"

# Check GPU
if ! command -v nvidia-smi &> /dev/null; then
    echo "ERROR: nvidia-smi not found. Ensure NVIDIA drivers are installed."
    echo "  For Azure NC/ND VMs, drivers should be pre-installed."
    echo "  If not: sudo apt install -y nvidia-driver-535"
    exit 1
fi

echo "GPU detected:"
nvidia-smi --query-gpu=name,memory.total,driver_version --format=csv,noheader
echo ""

# System packages
echo "Installing system dependencies..."
sudo apt-get update -qq
sudo apt-get install -y -qq python3-pip python3-venv openjdk-17-jdk curl git

# Java (for sbt/Scala)
java -version 2>&1 | head -1
echo ""

# sbt
if ! command -v sbt &> /dev/null; then
    echo "Installing sbt..."
    echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
    sudo apt-get update -qq
    sudo apt-get install -y -qq sbt
fi
sbt --version 2>&1 | head -1

# Python venv for vLLM
echo ""
echo "Creating Python virtual environment..."
python3 -m venv ~/vllm-env
source ~/vllm-env/bin/activate

echo "Installing vLLM and dependencies..."
pip install -q --upgrade pip
pip install -q vllm openai requests

echo "vLLM version: $(pip show vllm | grep Version)"

# Pre-download model weights (avoids timeout during benchmarks)
echo ""
echo "Pre-downloading LLaMA-2-7B weights..."
echo "  (This may take 10-20 minutes on first run)"
python3 -c "
from huggingface_hub import snapshot_download
snapshot_download('meta-llama/Llama-2-7b-hf', local_dir_use_symlinks=True)
print('Model downloaded successfully')
" 2>&1 || echo "WARNING: Model download failed. You may need HuggingFace auth:"
echo "  huggingface-cli login"

# Warm up sbt/compile Aura
echo ""
echo "Compiling Aura project..."
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_DIR"
sbt compile 2>&1 | tail -3

echo ""
echo "========================================================================"
echo "Setup complete. To run benchmarks:"
echo ""
echo "  source ~/vllm-env/bin/activate"
echo "  cd $PROJECT_DIR"
echo "  ./benchmarks/run_all.sh"
echo ""
echo "Or run individual phases:"
echo "  ./benchmarks/run_all.sh --skip-vllm    # skip real inference"
echo "  ./benchmarks/run_all.sh --skip-dvfs    # skip DVFS experiment"
echo "  ./benchmarks/run_all.sh --skip-aura    # skip simulator"
echo "========================================================================"

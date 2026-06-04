#!/usr/bin/env python3
"""Phase 4: Compare real vLLM measurements against Aura simulator predictions.

Reads CSVs from previous phases and produces:
  - comparison_latency.csv: TTFT/TPOT real vs predicted with error %
  - comparison_power.csv: power real vs predicted with error %
  - comparison_dvfs.csv: DVFS energy savings real vs predicted
  - paper_ready_tables.txt: LaTeX-formatted tables for the paper
"""

import csv
import os
import sys
from collections import defaultdict
from pathlib import Path

results_dir = Path(sys.argv[1])
output_dir = results_dir / "comparison"
output_dir.mkdir(exist_ok=True)

def read_csv(path):
    """Read CSV file, return list of dicts."""
    if not path.exists():
        return []
    with open(path) as f:
        return list(csv.DictReader(f))

def avg(values):
    return sum(values) / len(values) if values else 0

# ── Latency Comparison ──────────────────────────────────────────────────

print("=" * 70)
print("Latency Comparison: vLLM (real) vs Aura (simulator)")
print("=" * 70)

vllm_latency = read_csv(results_dir / "vllm_latency" / "vllm_latency.csv")
aura_fidelity = read_csv(results_dir / "aura_simulator" / "exp1_fidelity.csv")

if vllm_latency:
    # Group vLLM results by batch size, average across runs
    by_bs = defaultdict(list)
    for row in vllm_latency:
        bs = int(row['batch_size'])
        by_bs[bs].append(row)

    print(f"\n{'BS':>4s} {'vLLM TTFT':>10s} {'vLLM TPOT':>10s}")
    print("-" * 30)

    vllm_summary = {}
    for bs in sorted(by_bs.keys()):
        rows = by_bs[bs]
        ttft = avg([float(r['ttft_ms']) for r in rows])
        tpot = avg([float(r['tpot_ms']) for r in rows])
        vllm_summary[bs] = {'ttft': ttft, 'tpot': tpot}
        print(f"{bs:>4d} {ttft:>10.1f} {tpot:>10.2f}")

    # Write comparison CSV
    with open(output_dir / "comparison_latency.csv", 'w') as f:
        f.write("batch_size,vllm_ttft_ms,vllm_tpot_ms,aura_ttft_ms,aura_tpot_ms,"
                "ttft_error_pct,tpot_error_pct\n")
        for bs, v in sorted(vllm_summary.items()):
            # TODO: match with Aura predictions at same batch size
            f.write(f"{bs},{v['ttft']:.2f},{v['tpot']:.2f},,,\n")

    print(f"\nWritten: {output_dir / 'comparison_latency.csv'}")
else:
    print("No vLLM latency data found (skipped or not yet run)")

# ── Power Comparison ────────────────────────────────────────────────────

print("\n" + "=" * 70)
print("Power Comparison: nvidia-smi (real) vs Aura power model")
print("=" * 70)

power_data = read_csv(results_dir / "power_monitor" / "power_summary.csv")

if power_data:
    print(f"\n{'Scenario':>22s} {'BS':>4s} {'Avg W':>7s} {'Peak W':>7s}")
    print("-" * 45)
    for row in power_data:
        print(f"{row['scenario']:>22s} {row['batch_size']:>4s} "
              f"{float(row['avg_power_w']):>7.0f} {float(row['peak_power_w']):>7.0f}")

    with open(output_dir / "comparison_power.csv", 'w') as f:
        f.write("scenario,batch_size,real_avg_power_w,real_peak_power_w,"
                "aura_power_w,error_pct\n")
        for row in power_data:
            f.write(f"{row['scenario']},{row['batch_size']},"
                    f"{row['avg_power_w']},{row['peak_power_w']},,\n")

    print(f"\nWritten: {output_dir / 'comparison_power.csv'}")
else:
    print("No power data found (skipped or not yet run)")

# ── DVFS Comparison ─────────────────────────────────────────────────────

print("\n" + "=" * 70)
print("DVFS Experiment: Real PA-DVFS energy savings")
print("=" * 70)

dvfs_data = read_csv(results_dir / "dvfs_experiment" / "dvfs_all.csv")

if dvfs_data:
    by_freq = defaultdict(list)
    for row in dvfs_data:
        by_freq[row['freq_label']].append(row)

    baseline_eptoken = None
    baseline_tpot = None

    print(f"\n{'Freq':>8s} {'TPOT(ms)':>10s} {'Power(W)':>10s} {'mJ/tok':>10s} "
          f"{'E save':>8s} {'TPOT Δ':>8s}")
    print("-" * 60)

    dvfs_summary = []
    for label in ['100pct', '80pct', '60pct', '40pct']:
        if label not in by_freq:
            continue
        group = by_freq[label]
        t = avg([float(r['avg_tpot_ms']) for r in group])
        p = avg([float(r['avg_power_w']) for r in group])
        e = avg([float(r['energy_per_token_mj']) for r in group])
        freq = int(group[0]['freq_mhz'])

        if baseline_eptoken is None:
            baseline_eptoken = e
            baseline_tpot = t

        e_save = (1 - e / baseline_eptoken) * 100 if baseline_eptoken else 0
        t_delta = (t / baseline_tpot - 1) * 100 if baseline_tpot else 0

        print(f"{label:>8s} {t:>10.2f} {p:>10.0f} {e:>10.2f} "
              f"{e_save:>7.1f}% {t_delta:>7.1f}%")

        dvfs_summary.append({
            'freq_label': label, 'freq_mhz': freq,
            'avg_tpot_ms': t, 'avg_power_w': p,
            'energy_per_token_mj': e,
            'energy_saving_pct': e_save, 'tpot_delta_pct': t_delta,
        })

    with open(output_dir / "comparison_dvfs.csv", 'w') as f:
        f.write("freq_label,freq_mhz,avg_tpot_ms,avg_power_w,"
                "energy_per_token_mj,energy_saving_pct,tpot_delta_pct\n")
        for d in dvfs_summary:
            f.write(f"{d['freq_label']},{d['freq_mhz']},{d['avg_tpot_ms']:.2f},"
                    f"{d['avg_power_w']:.1f},{d['energy_per_token_mj']:.2f},"
                    f"{d['energy_saving_pct']:.1f},{d['tpot_delta_pct']:.1f}\n")

    print(f"\nWritten: {output_dir / 'comparison_dvfs.csv'}")

    # Key result for the paper
    if len(dvfs_summary) >= 3:
        best = min(dvfs_summary[1:], key=lambda d: d['energy_per_token_mj'])
        print(f"\n*** KEY RESULT ***")
        print(f"Best PA-DVFS point: {best['freq_label']} ({best['freq_mhz']} MHz)")
        print(f"  Energy saving: {best['energy_saving_pct']:.1f}%")
        print(f"  TPOT increase: {best['tpot_delta_pct']:.1f}%")
        print(f"  Ratio (save/cost): {best['energy_saving_pct'] / max(best['tpot_delta_pct'], 0.1):.1f}x")
else:
    print("No DVFS data found (skipped or not yet run)")

# ── Generate LaTeX Tables ───────────────────────────────────────────────

print("\n" + "=" * 70)
print("Generating LaTeX tables for paper...")
print("=" * 70)

latex_file = output_dir / "paper_tables.tex"
with open(latex_file, 'w') as f:
    f.write("% Auto-generated tables from Azure benchmark runs\n")
    f.write(f"% Generated from: {results_dir}\n\n")

    # DVFS table
    if dvfs_data:
        f.write("% Table: PA-DVFS Real Hardware Results\n")
        f.write("\\begin{table}[h]\n\\centering\n")
        f.write("\\caption{PA-DVFS energy savings on real A100 hardware "
                "(LLaMA-2-7B, bs=32, decode-heavy workload).}\n")
        f.write("\\label{tab:dvfs-real}\n")
        f.write("\\begin{tabular}{lcccc}\n\\toprule\n")
        f.write("\\textbf{Freq} & \\textbf{TPOT (ms)} & \\textbf{Power (W)} "
                "& \\textbf{mJ/tok} & \\textbf{E Saving} \\\\\n\\midrule\n")
        for d in dvfs_summary:
            save_str = f"{d['energy_saving_pct']:.1f}\\%" if d['energy_saving_pct'] > 0 else "baseline"
            f.write(f"{d['freq_mhz']} MHz & {d['avg_tpot_ms']:.1f} & "
                    f"{d['avg_power_w']:.0f} & {d['energy_per_token_mj']:.2f} & "
                    f"{save_str} \\\\\n")
        f.write("\\bottomrule\n\\end{tabular}\n\\end{table}\n\n")

    # Power comparison table
    if power_data:
        f.write("% Table: GPU Power Measurements\n")
        f.write("\\begin{table}[h]\n\\centering\n")
        f.write("\\caption{Measured GPU power across workload scenarios.}\n")
        f.write("\\label{tab:power-measured}\n")
        f.write("\\begin{tabular}{lccc}\n\\toprule\n")
        f.write("\\textbf{Scenario} & \\textbf{BS} & \\textbf{Avg W} "
                "& \\textbf{Peak W} \\\\\n\\midrule\n")
        for row in power_data:
            name = row['scenario'].replace('_', '\\_')
            f.write(f"{name} & {row['batch_size']} & "
                    f"{float(row['avg_power_w']):.0f} & "
                    f"{float(row['peak_power_w']):.0f} \\\\\n")
        f.write("\\bottomrule\n\\end{tabular}\n\\end{table}\n\n")

    print(f"Written: {latex_file}")

# ── Summary ─────────────────────────────────────────────────────────────

print("\n" + "=" * 70)
print("BENCHMARK SUMMARY")
print("=" * 70)

files = list(output_dir.glob("*.csv")) + list(output_dir.glob("*.tex"))
print(f"\nOutput files ({len(files)}):")
for f in sorted(files):
    size = f.stat().st_size
    print(f"  {f.name:>30s}  ({size:,} bytes)")

print("\nNext steps:")
print("  1. Review comparison CSVs for model calibration accuracy")
print("  2. If errors > 10%, adjust Aura power/latency model coefficients")
print("  3. Copy paper_tables.tex content into Overleaf sections/evaluation.tex")
print("  4. Generate plots using the CSV data (matplotlib or pgfplots)")

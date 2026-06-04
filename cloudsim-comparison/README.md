# CloudSim Plus Comparison Benchmark

Standalone benchmark project that implements the same 4 IaaS scenarios (S1-S4)
used in **Aura**, but using **CloudSim Plus 8.0**, for direct performance
comparison.

## Scenarios

| ID | Name     | DCs | Hosts | VMs   | Workloads | Description            |
|----|----------|-----|-------|-------|-----------|------------------------|
| S1 | Small    | 1   | 10    | 100   | 1,000     | Baseline sanity check  |
| S2 | Medium   | 1   | 100   | 500   | 5,000     | Moderate scale          |
| S3 | Large    | 1   | 1,000 | 5,000 | 50,000    | Stress test             |
| S4 | Multi-DC | 5   | 500*  | 2,000 | 10,000    | Multi-datacenter        |

*S4 uses 5 DCs with 100 hosts each (500 total).

All hosts: 16 PEs @ 20,000 MIPS, 64 GB RAM.
All VMs: 2 PEs @ 2,000 MIPS, 4 GB RAM.
All workloads: 20,000 MI per cloudlet.

## Prerequisites

- **Java 17+** (tested with OpenJDK 17/21)
- **Gradle 8+** (or use the included wrapper if you generate one)

## Quick Start

### Using Gradle directly

```bash
# Run all 4 scenarios (10 iterations each)
gradle run

# Skip the large and multi-DC scenarios (for quick sanity checks)
gradle run -Dskip.large=true
```

### Using the Gradle wrapper (recommended)

Generate a wrapper first if one doesn't exist:

```bash
gradle wrapper --gradle-version 8.5
./gradlew run
```

### Building a fat JAR

```bash
gradle fatJar
java -Xmx8g -jar build/libs/cloudsim-comparison-1.0.0-all.jar
```

## Output Format

Progress and per-iteration details are printed to **stderr**.
The CSV results are printed to **stdout**, making it easy to redirect:

```bash
gradle run 2>/dev/null > results.csv
```

CSV columns:

```
Scenario,Median_Time_ms,StdDev_Time_ms,Median_Peak_MB,StdDev_Peak_MB
```

## Comparing with Aura

1. Run this benchmark and capture `cloudsim_results.csv`.
2. Run the equivalent Aura benchmark and capture `aura_results.csv`.
3. Compare median times and memory usage side by side.

## Configuration

All scenario parameters are defined as constants at the top of
`CloudSimPlusBenchmark.java`. You can adjust:

- `ITERATIONS` — number of repetitions per scenario (default: 10)
- Host/VM/Cloudlet specs — PE counts, MIPS, RAM, etc.
- Scheduling policies — currently uses `VmAllocationPolicyBestFit` and
  `CloudletSchedulerTimeShared`

## Notes on Memory Measurement

The benchmark measures memory via `Runtime.getRuntime()` (used minus free).
This is an approximation; for precise measurements consider using a profiler
or JMX `MemoryMXBean`. The GC is explicitly triggered before each iteration
for better consistency, but results may still vary between JVM implementations.

## Dependency

This project depends on:

```
org.cloudsimplus:cloudsim-plus:8.0.0
```

If version 8.0.0 is not available on Maven Central, update the version in
`build.gradle` to the latest release. Check
[Maven Central](https://central.sonatype.com/artifact/org.cloudsimplus/cloudsim-plus)
for the current latest version.

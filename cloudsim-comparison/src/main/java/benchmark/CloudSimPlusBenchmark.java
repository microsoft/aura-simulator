package benchmark;

import org.cloudsimplus.brokers.DatacenterBroker;
import org.cloudsimplus.brokers.DatacenterBrokerSimple;
import org.cloudsimplus.cloudlets.Cloudlet;
import org.cloudsimplus.cloudlets.CloudletSimple;
import org.cloudsimplus.core.CloudSimPlus;
import org.cloudsimplus.datacenters.Datacenter;
import org.cloudsimplus.datacenters.DatacenterSimple;
import org.cloudsimplus.hosts.Host;
import org.cloudsimplus.hosts.HostSimple;
import org.cloudsimplus.resources.Pe;
import org.cloudsimplus.resources.PeSimple;
import org.cloudsimplus.schedulers.cloudlet.CloudletSchedulerSpaceShared;
import org.cloudsimplus.schedulers.cloudlet.CloudletSchedulerTimeShared;
import org.cloudsimplus.schedulers.vm.VmSchedulerSpaceShared;
import org.cloudsimplus.schedulers.vm.VmSchedulerTimeShared;
import org.cloudsimplus.allocationpolicies.VmAllocationPolicyBestFit;
import org.cloudsimplus.utilizationmodels.UtilizationModelFull;
import org.cloudsimplus.vms.Vm;
import org.cloudsimplus.vms.VmSimple;

import java.lang.management.ManagementFactory;
import java.lang.management.MemoryPoolMXBean;
import java.lang.management.MemoryType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * CloudSim Plus benchmark implementing 4 IaaS scenarios (S1-S4) for direct
 * performance comparison with the Aura simulator.
 *
 * Each scenario is run {@code ITERATIONS} times. The benchmark reports
 * median execution time (ms) and peak memory (MB) with standard deviation,
 * printed in CSV format: Scenario,Time_ms,Peak_MB
 */
public class CloudSimPlusBenchmark {

    // ── Scenario Constants ────────────────────────────────────────────────

    // Host specification (common to all scenarios)
    private static final int HOST_PES       = 16;
    private static final long HOST_MIPS     = 20_000L;
    private static final long HOST_RAM_MB   = 64_000L;   // 64 GB
    private static final long HOST_BW_MBPS  = 10_000L;
    private static final long HOST_STORAGE  = 1_000_000L; // 1 TB

    // VM specification (common to all scenarios)
    private static final int VM_PES         = 2;
    private static final long VM_MIPS       = 2_000L;
    private static final long VM_RAM_MB     = 4_000L;     // 4 GB
    private static final long VM_BW_MBPS    = 1_000L;
    private static final long VM_STORAGE    = 100_000L;   // 100 GB

    // Cloudlet (workload) specification
    private static final long CLOUDLET_LENGTH = 20_000L;  // MI
    private static final int  CLOUDLET_PES    = 1;

    // Benchmark control
    private static final int ITERATIONS = 10;

    // ── Scenario Definitions ──────────────────────────────────────────────

    /**
     * Encapsulates the parameters for a single benchmark scenario.
     */
    private record ScenarioConfig(
            String name,
            int numDatacenters,
            int hostsPerDc,
            int numVms,
            int numCloudlets
    ) {}

    private static final ScenarioConfig[] SCENARIOS = {
            new ScenarioConfig("S1_Small",    1,   10,   100,   1_000),
            new ScenarioConfig("S2_Medium",   1,  100,   500,   5_000),
            new ScenarioConfig("S3_Large",    1, 1000,  5000,  50_000),
            new ScenarioConfig("S4_MultiDC",  5,  100,  2000,  10_000),
    };

    // ── Entry Point ───────────────────────────────────────────────────────

    public static void main(String[] args) {
        // If "validate" argument is passed, run functional correctness validation
        if (args.length > 0 && "validate".equals(args[0])) {
            runValidation();
            return;
        }

        System.out.println("CloudSim Plus Benchmark — Aura Comparison");
        System.out.println("=============================================");
        System.out.printf("Iterations per scenario: %d%n%n", ITERATIONS);

        // CSV header
        System.out.println("Scenario,Median_Time_ms,StdDev_Time_ms,Median_Peak_MB,StdDev_Peak_MB");

        for (ScenarioConfig scenario : SCENARIOS) {
            // Optionally skip large scenarios via system property
            if ("true".equals(System.getProperty("skip.large"))
                    && (scenario.name.contains("Large") || scenario.name.contains("MultiDC"))) {
                System.err.printf("[SKIP] %s (skip.large is set)%n", scenario.name);
                continue;
            }

            runScenarioBenchmark(scenario);
        }
    }

    // ── Functional Correctness Validation ────────────────────────────────

    /**
     * Runs a controlled validation scenario and prints per-cloudlet results
     * in CSV format, plus aggregate metrics, for comparison with Aura.
     *
     * Uses 1-PE VMs with exactly 1 cloudlet per VM, eliminating any
     * MIPS-per-PE vs total-MIPS ambiguity and scheduler-dependent behaviour.
     * Configuration:
     *   100 hosts (16 PEs, 2K MIPS/PE), 1,000 VMs (1 PE, 2K MIPS),
     *   1,000 cloudlets (1 PE, 20K MI) — 1 per VM.
     * Expected: all complete at ~10s (20000 MI / 2000 MIPS = 10s).
     */
    private static void runValidation() {
        System.out.println("CloudSim Plus — Functional Correctness Validation");
        System.out.println("=================================================");
        System.out.println("Config: 100 hosts, 1000 VMs (1-PE, 2000 MIPS), 1000 workloads (1 per VM)");
        System.out.println();

        // Validation-specific parameters
        final int VAL_NUM_HOSTS     = 100;
        final int VAL_HOST_PES      = 16;
        final long VAL_HOST_PE_MIPS = 2_000L;  // per PE — matches VM MIPS exactly
        final int VAL_NUM_VMS       = 1000;
        final int VAL_NUM_CLOUDLETS = 1000;
        final int VAL_VM_PES        = 1;
        final long VAL_VM_MIPS      = 2_000L;

        CloudSimPlus simulation = new CloudSimPlus();

        // Create datacenter with SpaceShared VM scheduler.
        // Each host has 16 PEs at 2000 MIPS → can hold 16 single-PE VMs.
        // 100 hosts × 16 = 1600 VM slots > 1000 VMs needed.
        List<Host> hostList = new ArrayList<>(VAL_NUM_HOSTS);
        for (int h = 0; h < VAL_NUM_HOSTS; h++) {
            List<Pe> peList = new ArrayList<>(VAL_HOST_PES);
            for (int p = 0; p < VAL_HOST_PES; p++) {
                peList.add(new PeSimple(VAL_HOST_PE_MIPS));
            }
            Host host = new HostSimple(HOST_RAM_MB * 10, HOST_BW_MBPS * 10,
                    HOST_STORAGE * 10, peList);
            host.setVmScheduler(new VmSchedulerSpaceShared());
            hostList.add(host);
        }
        new DatacenterSimple(simulation, hostList, new VmAllocationPolicyBestFit());

        // Create broker
        DatacenterBroker broker = new DatacenterBrokerSimple(simulation);

        // Create 1000 single-PE VMs at 2000 MIPS
        List<Vm> vmList = new ArrayList<>(VAL_NUM_VMS);
        for (int i = 0; i < VAL_NUM_VMS; i++) {
            Vm vm = new VmSimple(VAL_VM_MIPS, VAL_VM_PES);
            vm.setRam(VM_RAM_MB).setBw(VM_BW_MBPS).setSize(VM_STORAGE)
              .setCloudletScheduler(new CloudletSchedulerSpaceShared());
            vmList.add(vm);
        }
        broker.submitVmList(vmList);

        // Create cloudlets (1 PE each, 20K MI) — broker distributes round-robin
        List<Cloudlet> cloudletList = createCloudlets(VAL_NUM_CLOUDLETS);
        broker.submitCloudletList(cloudletList);

        // Run simulation
        long startNs = System.nanoTime();
        simulation.start();
        long elapsedMs = (System.nanoTime() - startNs) / 1_000_000;

        // Extract results
        List<Cloudlet> finished = broker.getCloudletFinishedList();

        // Per-cloudlet CSV
        System.out.println("cloudlet_id,vm_id,host_id,finish_time,cpu_time,mi_executed,status");
        double sumFinishTime = 0.0;
        double minFinishTime = Double.MAX_VALUE;
        double maxFinishTime = Double.MIN_VALUE;
        int completed = 0;
        int failed = 0;

        for (Cloudlet cl : finished) {
            long cloudletId = cl.getId();
            long vmId = cl.getVm().getId();
            long hostId = cl.getVm().getHost().getId();
            double finishTime = cl.getFinishTime();
            double cpuTime = cl.getActualCpuTime();
            long miExecuted = cl.getLength();
            String status = cl.getStatus().name();

            System.out.printf("%d,%d,%d,%.2f,%.2f,%d,%s%n",
                    cloudletId, vmId, hostId, finishTime, cpuTime, miExecuted, status);

            if (finishTime > 0) {
                completed++;
                sumFinishTime += finishTime;
                minFinishTime = Math.min(minFinishTime, finishTime);
                maxFinishTime = Math.max(maxFinishTime, finishTime);
            } else {
                failed++;
            }
        }

        // Aggregate metrics
        double meanFinishTime = completed > 0 ? sumFinishTime / completed : 0.0;

        System.out.println();
        System.out.println("=== Aggregate Metrics ===");
        System.out.printf("Total completed: %d%n", completed);
        System.out.printf("Total failed:    %d%n", failed);
        System.out.printf("Mean finish time: %.4f s%n", meanFinishTime);
        System.out.printf("Min finish time:  %.4f s%n", minFinishTime == Double.MAX_VALUE ? 0.0 : minFinishTime);
        System.out.printf("Max finish time:  %.4f s%n", maxFinishTime == Double.MIN_VALUE ? 0.0 : maxFinishTime);
        System.out.printf("Execution time:   %d ms%n", elapsedMs);
    }

    // ── Benchmark Harness ─────────────────────────────────────────────────

    // JMX peak heap measurement: capture true peak across all heap pools during run.
    private static final List<MemoryPoolMXBean> HEAP_POOLS =
            ManagementFactory.getMemoryPoolMXBeans().stream()
                    .filter(p -> p.getType() == MemoryType.HEAP)
                    .collect(Collectors.toList());

    private static void resetPeakHeap() {
        for (MemoryPoolMXBean p : HEAP_POOLS) p.resetPeakUsage();
    }

    private static long readPeakHeapBytes() {
        long total = 0L;
        for (MemoryPoolMXBean p : HEAP_POOLS) total += p.getPeakUsage().getUsed();
        return total;
    }

    private static void runScenarioBenchmark(ScenarioConfig config) {
        double[] times       = new double[ITERATIONS];
        double[] peakHeapMb  = new double[ITERATIONS];
        double[] steadyMb    = new double[ITERATIONS];

        System.err.printf("[INFO] Running %s (%d DCs, %d hosts, %d VMs, %d workloads)...%n",
                config.name, config.numDatacenters, config.hostsPerDc,
                config.numVms, config.numCloudlets);

        for (int i = 0; i < ITERATIONS; i++) {
            // Stabilize before measurement
            System.gc();
            try { Thread.sleep(200); } catch (InterruptedException ignored) {}
            System.gc();
            try { Thread.sleep(100); } catch (InterruptedException ignored) {}

            // Reset JMX peak counters after GC so we capture allocation during the run only
            resetPeakHeap();

            long startNs = System.nanoTime();
            runSimulation(config);
            long endNs = System.nanoTime();

            long peakBytes = readPeakHeapBytes();

            // Steady-state heap after GC (old measurement, kept for comparison)
            System.gc();
            try { Thread.sleep(100); } catch (InterruptedException ignored) {}
            Runtime runtime = Runtime.getRuntime();
            long steadyBytes = runtime.totalMemory() - runtime.freeMemory();

            times[i]      = (endNs - startNs) / 1_000_000.0;             // ms
            peakHeapMb[i] = peakBytes  / (1024.0 * 1024.0);              // MB
            steadyMb[i]   = steadyBytes / (1024.0 * 1024.0);             // MB

            System.err.printf("  [%s] iter %2d/%d  time=%.1f ms  peak=%.1f MB  steady=%.1f MB%n",
                    config.name, i + 1, ITERATIONS, times[i], peakHeapMb[i], steadyMb[i]);
        }

        double medianTime  = median(times);
        double stdTime     = stddev(times);
        double medianPeak  = median(peakHeapMb);
        double stdPeak     = stddev(peakHeapMb);
        double medianSteady = median(steadyMb);

        // CSV row: name,time_ms,std_time,peak_heap_MB,std_peak,steady_MB
        System.out.printf("%s,%.2f,%.2f,%.2f,%.2f,%.2f%n",
                config.name, medianTime, stdTime, medianPeak, stdPeak, medianSteady);
    }

    // ── CloudSim Plus Simulation ──────────────────────────────────────────

    private static void runSimulation(ScenarioConfig config) {
        CloudSimPlus simulation = new CloudSimPlus();

        // Create datacenter(s)
        List<Datacenter> datacenters = new ArrayList<>(config.numDatacenters);
        for (int dc = 0; dc < config.numDatacenters; dc++) {
            datacenters.add(createDatacenter(simulation, config.hostsPerDc));
        }

        // Create broker
        DatacenterBroker broker = new DatacenterBrokerSimple(simulation);

        // Create VMs
        List<Vm> vmList = createVms(config.numVms);
        broker.submitVmList(vmList);

        // Create cloudlets (workloads)
        List<Cloudlet> cloudletList = createCloudlets(config.numCloudlets);
        broker.submitCloudletList(cloudletList);

        // Run
        simulation.start();

        // We don't print individual cloudlet results — only timing matters.
    }

    private static Datacenter createDatacenter(CloudSimPlus simulation, int numHosts) {
        List<Host> hostList = new ArrayList<>(numHosts);
        for (int i = 0; i < numHosts; i++) {
            hostList.add(createHost());
        }
        return new DatacenterSimple(simulation, hostList, new VmAllocationPolicyBestFit());
    }

    private static Host createHost() {
        List<Pe> peList = new ArrayList<>(HOST_PES);
        for (int i = 0; i < HOST_PES; i++) {
            peList.add(new PeSimple(HOST_MIPS));
        }

        Host host = new HostSimple(HOST_RAM_MB, HOST_BW_MBPS, HOST_STORAGE, peList);
        host.setVmScheduler(new VmSchedulerTimeShared());
        return host;
    }

    private static List<Vm> createVms(int count) {
        List<Vm> list = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            Vm vm = new VmSimple(VM_MIPS, VM_PES);
            vm.setRam(VM_RAM_MB)
              .setBw(VM_BW_MBPS)
              .setSize(VM_STORAGE)
              .setCloudletScheduler(new CloudletSchedulerTimeShared());
            list.add(vm);
        }
        return list;
    }

    private static List<Cloudlet> createCloudlets(int count) {
        UtilizationModelFull utilizationModel = new UtilizationModelFull();
        List<Cloudlet> list = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            Cloudlet cl = new CloudletSimple(CLOUDLET_LENGTH, CLOUDLET_PES);
            cl.setUtilizationModel(utilizationModel);
            list.add(cl);
        }
        return list;
    }

    // ── Statistics Helpers ─────────────────────────────────────────────────

    private static double median(double[] values) {
        double[] sorted = values.clone();
        Arrays.sort(sorted);
        int n = sorted.length;
        if (n % 2 == 0) {
            return (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0;
        } else {
            return sorted[n / 2];
        }
    }

    private static double stddev(double[] values) {
        double mean = Arrays.stream(values).average().orElse(0.0);
        double variance = Arrays.stream(values)
                .map(v -> (v - mean) * (v - mean))
                .average()
                .orElse(0.0);
        return Math.sqrt(variance);
    }
}

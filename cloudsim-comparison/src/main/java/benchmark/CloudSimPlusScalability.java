package benchmark;

import org.cloudsimplus.brokers.DatacenterBrokerSimple;
import org.cloudsimplus.cloudlets.Cloudlet;
import org.cloudsimplus.cloudlets.CloudletSimple;
import org.cloudsimplus.core.CloudSimPlus;
import org.cloudsimplus.datacenters.DatacenterSimple;
import org.cloudsimplus.hosts.Host;
import org.cloudsimplus.hosts.HostSimple;
import org.cloudsimplus.resources.Pe;
import org.cloudsimplus.resources.PeSimple;
import org.cloudsimplus.schedulers.cloudlet.CloudletSchedulerTimeShared;
import org.cloudsimplus.schedulers.vm.VmSchedulerTimeShared;
import org.cloudsimplus.allocationpolicies.VmAllocationPolicyBestFit;
import org.cloudsimplus.utilizationmodels.UtilizationModelFull;
import org.cloudsimplus.vms.Vm;
import org.cloudsimplus.vms.VmSimple;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * CloudSim Plus scalability benchmark for IEEE CLOUD 2026 paper comparison.
 *
 * Fixed infrastructure: 1 DC, 100 hosts, 500 VMs.
 * Varies workload count: 100, 500, 1000, 2000, 5000, 10000, 20000, 50000.
 * Reports median execution time and peak memory over 3 iterations.
 *
 * Run with: mvn exec:java -Dexec.mainClass="benchmark.CloudSimPlusScalability"
 */
public class CloudSimPlusScalability {

    private static final int HOST_PES      = 16;
    private static final long HOST_MIPS    = 20_000L;
    private static final long HOST_RAM_MB  = 64_000L;
    private static final long HOST_BW_MBPS = 10_000L;
    private static final long HOST_STORAGE = 1_000_000L;

    private static final int VM_PES        = 2;
    private static final long VM_MIPS      = 2_000L;
    private static final long VM_RAM_MB    = 4_000L;
    private static final long VM_BW_MBPS   = 1_000L;
    private static final long VM_STORAGE   = 100_000L;

    private static final long CLOUDLET_LENGTH = 20_000L;
    private static final int  CLOUDLET_PES    = 1;

    private static final int NUM_HOSTS = 100;
    private static final int NUM_VMS   = 500;
    private static final int ITERATIONS = 3;

    private static final int[] WORKLOAD_COUNTS = {100, 500, 1000, 2000, 5000, 10000, 20000, 50000};

    public static void main(String[] args) {
        System.out.println("CloudSim Plus Scalability Benchmark");
        System.out.println("Fixed: 1 DC, 100 hosts, 500 VMs | Varying workloads");
        System.out.println("===================================================");

        // Warmup
        System.err.println("[INFO] Warming up...");
        for (int i = 0; i < 3; i++) {
            runSimulation(50);
        }

        System.out.printf("%10s | %12s | %12s%n", "Workloads", "Time (ms)", "Memory (MB)");
        System.out.println("------------------------------------------");

        System.out.println();
        System.out.println("CSV output:");
        System.out.println("Workloads,Time_ms,Peak_MB");

        for (int count : WORKLOAD_COUNTS) {
            double[] times = new double[ITERATIONS];
            double[] mems  = new double[ITERATIONS];

            for (int i = 0; i < ITERATIONS; i++) {
                System.gc();
                try { Thread.sleep(300); } catch (InterruptedException ignored) {}
                System.gc();

                Runtime rt = Runtime.getRuntime();
                long memBefore = rt.totalMemory() - rt.freeMemory();
                long start = System.nanoTime();

                runSimulation(count);

                long elapsed = (System.nanoTime() - start) / 1_000_000;
                System.gc();
                try { Thread.sleep(200); } catch (InterruptedException ignored) {}
                long memAfter = rt.totalMemory() - rt.freeMemory();
                long peakUsed = Math.max(memAfter, memBefore);

                times[i] = elapsed;
                mems[i]  = peakUsed / (1024.0 * 1024.0);
            }

            Arrays.sort(times);
            Arrays.sort(mems);
            long medianTime = (long) times[ITERATIONS / 2];
            long medianMem  = (long) mems[ITERATIONS / 2];

            System.err.printf("%10d | %,12d | %,12d%n", count, medianTime, medianMem);
            System.out.printf("%d,%d,%d%n", count, medianTime, medianMem);
        }
    }

    private static void runSimulation(int numCloudlets) {
        CloudSimPlus simulation = new CloudSimPlus();

        List<Host> hostList = new ArrayList<>(NUM_HOSTS);
        for (int i = 0; i < NUM_HOSTS; i++) {
            List<Pe> peList = new ArrayList<>(HOST_PES);
            for (int j = 0; j < HOST_PES; j++) {
                peList.add(new PeSimple(HOST_MIPS));
            }
            Host host = new HostSimple(HOST_RAM_MB, HOST_BW_MBPS, HOST_STORAGE, peList);
            host.setVmScheduler(new VmSchedulerTimeShared());
            hostList.add(host);
        }
        new DatacenterSimple(simulation, hostList, new VmAllocationPolicyBestFit());

        var broker = new DatacenterBrokerSimple(simulation);

        List<Vm> vmList = new ArrayList<>(NUM_VMS);
        for (int i = 0; i < NUM_VMS; i++) {
            Vm vm = new VmSimple(VM_MIPS, VM_PES);
            vm.setRam(VM_RAM_MB).setBw(VM_BW_MBPS).setSize(VM_STORAGE)
              .setCloudletScheduler(new CloudletSchedulerTimeShared());
            vmList.add(vm);
        }
        broker.submitVmList(vmList);

        UtilizationModelFull util = new UtilizationModelFull();
        List<Cloudlet> cloudletList = new ArrayList<>(numCloudlets);
        for (int i = 0; i < numCloudlets; i++) {
            Cloudlet cl = new CloudletSimple(CLOUDLET_LENGTH, CLOUDLET_PES);
            cl.setUtilizationModel(util);
            cloudletList.add(cl);
        }
        broker.submitCloudletList(cloudletList);

        simulation.start();
    }
}

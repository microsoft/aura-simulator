version      := "1.0.0"
scalaVersion := "3.9.0"
organization := "io.aura"

// ─── Global settings ───────────────────────────────────────────────────────────
scalacOptions ++= Seq(
  "-Wunused:all",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:implicitConversions"
)

// Fork tests in a separate JVM to avoid Pekko classloader/thread issues
Test / fork               := true
Test / outputStrategy     := Some(StdoutOutput)
Test / parallelExecution  := false
Test / javaOptions ++= Seq("-Xmx1g", "-Xss2m")

// Prevent parallel test execution across modules to avoid actor system resource contention
Global / concurrentRestrictions += Tags.limit(Tags.Test, 1)

val PekkoVersion      = "1.1.3"
val ScalaTestVersion  = "3.2.20"
val ScalaCheckVersion = "1.20.0"

lazy val commonDeps = Seq(
  "org.scalatest"     %% "scalatest"       % ScalaTestVersion  % Test,
  "org.scalacheck"    %% "scalacheck"      % ScalaCheckVersion % Test,
  "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0"        % Test
)

lazy val pekkoDeps = Seq(
  "org.apache.pekko" %% "pekko-actor-typed"         % PekkoVersion,
  "org.apache.pekko" %% "pekko-actor-testkit-typed" % PekkoVersion % Test,
  "org.apache.pekko" %% "pekko-slf4j"               % PekkoVersion,
  "ch.qos.logback"    % "logback-classic"           % "1.6.3"
)

// ─── Root ──────────────────────────────────────────────────────────────────────
lazy val root = (project in file("."))
  .aggregate(
    auraCore,
    auraIaas,
    auraDsl,
    auraServerless,
    auraContainers,
    auraEdge,
    auraPower,
    auraGpu,
    auraInference,
    auraTraces,
    auraBench,
    auraViz,
    auraVizFrontend,
    auraExamples
  )
  .settings(
    name           := "aura",
    publish / skip := true
  )

// ─── Core: DES engine, opaque types, event system ─────────────────────────────
lazy val auraCore = (project in file("modules/aura-core"))
  .settings(
    name := "aura-core",
    libraryDependencies ++= commonDeps ++ pekkoDeps
  )

// ─── IaaS: Datacenter, Host, VM, Workload actors ─────────────────────────────
lazy val auraIaas = (project in file("modules/aura-iaas"))
  .dependsOn(auraCore, auraPower)
  .settings(
    name := "aura-iaas",
    libraryDependencies ++= commonDeps ++ pekkoDeps
  )

// ─── DSL: Simulation configuration ────────────────────────────────────────────
lazy val auraDsl = (project in file("modules/aura-dsl"))
  .dependsOn(auraCore, auraIaas, auraPower, auraServerless, auraContainers, auraEdge, auraGpu, auraInference)
  .settings(
    name := "aura-dsl",
    libraryDependencies ++= commonDeps ++ pekkoDeps
  )

// ─── Serverless: FaaS platform, cold starts, auto-scaling ────────────────────
lazy val auraServerless = (project in file("modules/aura-serverless"))
  .dependsOn(auraCore, auraIaas)
  .settings(
    name := "aura-serverless",
    libraryDependencies ++= commonDeps ++ pekkoDeps
  )

// ─── Containers: Container, Pod, K8s scheduler ───────────────────────────────
lazy val auraContainers = (project in file("modules/aura-containers"))
  .dependsOn(auraCore, auraIaas)
  .settings(
    name := "aura-containers",
    libraryDependencies ++= commonDeps ++ pekkoDeps
  )

// ─── Edge: Edge nodes, latency models, offloading policies ───────────────────
lazy val auraEdge = (project in file("modules/aura-edge"))
  .dependsOn(auraCore, auraIaas)
  .settings(
    name := "aura-edge",
    libraryDependencies ++= commonDeps ++ pekkoDeps
  )

// ─── Power: Compositional power models ────────────────────────────────────────
lazy val auraPower = (project in file("modules/aura-power"))
  .dependsOn(auraCore)
  .settings(
    name := "aura-power",
    libraryDependencies ++= commonDeps
  )

// ─── GPU: GPU hardware substrate, power models, DVFS ─────────────────────────
lazy val auraGpu = (project in file("modules/aura-gpu"))
  .dependsOn(auraCore, auraPower)
  .settings(
    name := "aura-gpu",
    libraryDependencies ++= commonDeps ++ pekkoDeps
  )

// ─── Inference: LLM inference simulation, energy-aware scheduling ────────────
lazy val auraInference = (project in file("modules/aura-inference"))
  .dependsOn(auraCore, auraPower, auraGpu)
  .settings(
    name := "aura-inference",
    libraryDependencies ++= commonDeps ++ pekkoDeps
  )

// ─── Traces: Google Cluster Data + Azure VM Traces readers ───────────────────
lazy val auraTraces = (project in file("modules/aura-traces"))
  .dependsOn(auraCore)
  .settings(
    name := "aura-traces",
    libraryDependencies ++= commonDeps
  )

// ─── Benchmarks: JMH ─────────────────────────────────────────────────────────
lazy val auraBench = (project in file("modules/aura-bench"))
  .dependsOn(auraCore, auraIaas, auraPower, auraDsl, auraServerless, auraContainers, auraEdge, auraGpu, auraInference)
  .enablePlugins(JmhPlugin)
  .settings(
    name           := "aura-bench",
    publish / skip := true
  )

// ─── Viz Frontend: Scala.js + Laminar SPA ────────────────────────────────────
lazy val auraVizFrontend = (project in file("modules/aura-viz-frontend"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name                            := "aura-viz-frontend",
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.NoModule) },
    Test / fork := false,
    libraryDependencies ++= Seq(
      "org.scala-js"  %% "scalajs-dom" % "2.8.0",
      "com.raquo"     %% "laminar"     % "17.2.0",
      "com.lihaoyi"   %% "upickle"     % "4.0.2",
      "org.scalatest" %% "scalatest"   % ScalaTestVersion % Test
    )
  )

// ─── Viz: Dashboard & report generation ──────────────────────────────────────
lazy val auraViz = (project in file("modules/aura-viz"))
  .dependsOn(auraCore)
  .settings(
    name := "aura-viz",
    libraryDependencies ++= commonDeps,
    Compile / resourceGenerators += Def.task {
      (auraVizFrontend / Compile / fullLinkJS).value // trigger Scala.js linking
      val jsDir  = (auraVizFrontend / Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value
      val jsFile = jsDir / "main.js"
      val target = (Compile / resourceManaged).value / "dashboard" / "js" / "app.js"
      Def.uncached {
        IO.copyFile(jsFile, target)
        Seq(target)
      }
    }.taskValue
  )

// ─── Examples: Example simulations ────────────────────────────────────────────
lazy val auraExamples = (project in file("modules/aura-examples"))
  .dependsOn(
    auraCore,
    auraIaas,
    auraDsl,
    auraPower,
    auraServerless,
    auraContainers,
    auraEdge,
    auraGpu,
    auraInference,
    auraViz,
    auraTraces
  )
  .settings(
    name := "aura-examples",
    libraryDependencies ++= commonDeps ++ pekkoDeps,
    publish / skip := true
  )

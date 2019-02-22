
inThisBuild(Seq(
  scalaVersion := "2.12.6",
  organization := "com.poligon",
  compileOrder := CompileOrder.JavaThenScala,
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:dynamics",
    "-language:experimental.macros",
    "-language:higherKinds",
    "-Xfuture",
    "-Xfatal-warnings",
    "-Xlint:_,-missing-interpolator,-adapted-args"
  ),
  javacOptions := Seq(
    "-source", "1.8",
    "-target", "1.8",
    "-parameters"
  ),
  autoAPIMappings := true,
  libraryDependencies ++= Seq(
    compilerPlugin("com.github.ghik" %% "silencer-plugin" % silencerVersion),
    "com.github.ghik" %% "silencer-lib" % silencerVersion % Provided
  )
))

val silencerVersion = "1.2.1"
val commonsVersion = "1.34.0"
val forIdeaImport = System.getProperty("idea.managed", "false").toBoolean && System.getProperty("idea.runid") == null

def mkSourceDirs(base: File, scalaBinary: String, conf: String): Seq[File] = Seq(
  base / "src" / conf / "scala",
  base / "src" / conf / s"scala-$scalaBinary",
  base / "src" / conf / "java"
)

def mkResourceDirs(base: File, conf: String): Seq[File] = Seq(
  base / "src" / conf / "resources"
)

def sourceDirsSettings(baseMapper: File => File) = Seq(
  Compile / unmanagedSourceDirectories ++=
    mkSourceDirs(baseMapper(baseDirectory.value), scalaBinaryVersion.value, "main"),
  Compile / unmanagedResourceDirectories ++=
    mkResourceDirs(baseMapper(baseDirectory.value), "main"),
  Test / unmanagedSourceDirectories ++=
    mkSourceDirs(baseMapper(baseDirectory.value), scalaBinaryVersion.value, "test"),
  Test / unmanagedResourceDirectories ++=
    mkResourceDirs(baseMapper(baseDirectory.value), "test"),
)

def jsProjectFor(parentProj: Project, jvmProj: Project): Project =
  parentProj.in(jvmProj.base / ".js")
    .enablePlugins(ScalaJSPlugin)
    .configure(p => if (forIdeaImport) p.dependsOn(jvmProj) else p)
    .settings(
      moduleName := (jvmProj / moduleName).value,
      sourceDirsSettings(_.getParentFile),
      // workaround for some cross-compilation problems in IntelliJ
      libraryDependencies := (if (forIdeaImport) (jvmProj / libraryDependencies).value else Seq.empty) ++ libraryDependencies.value
    )

val basicDeps = Def.setting(Seq(
  "com.avsystem.commons" %% "commons-core" % commonsVersion,
  "com.avsystem.commons" %% "commons-spring" % commonsVersion,
  "com.avsystem.commons" %% "commons-mongo" % commonsVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  "org.scalatest" % "scalatest_2.12" % "3.0.4" % Test
))

lazy val `poligon-macros` = project
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    libraryDependencies += "com.avsystem.commons" %% "commons-macros" % commonsVersion,
  )

lazy val `poligon-scalaConfig` = project.dependsOn(`poligon-macros` % "compile->compile;test->test")
  .settings(libraryDependencies ++= basicDeps.value)

lazy val `poligon-functional` = project.dependsOn(`poligon-macros` % "compile->compile;test->test")
  .settings(libraryDependencies ++= basicDeps.value)
  .settings(libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.4.1")
  .settings(libraryDependencies += "org.mongodb.scala" %% "mongo-scala-driver" % "2.4.2")

lazy val `poligon-properties` = project.dependsOn(`poligon-macros`)
  .settings(libraryDependencies += "com.avsystem.commons" %% "commons-core" % commonsVersion)
  .settings(libraryDependencies += "io.monix" %% "monix" % "2.3.3")
  .settings(libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % Test)

lazy val `poligon-properties-js` = jsProjectFor(project, `poligon-properties`).dependsOn(`poligon-macros`)
  .settings(libraryDependencies += "com.avsystem.commons" %%% "commons-core" % commonsVersion)
  .settings(libraryDependencies += "io.monix" %%% "monix" % "2.3.3")
  .settings(libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % Test)

lazy val `poligon-exampleapp` = project.dependsOn(`poligon-properties`)

lazy val `poligon-exampleapp-js` = jsProjectFor(project, `poligon-exampleapp`)
  .dependsOn(`poligon-properties-js`)

lazy val `poligon-scalajs` = project.dependsOn(`poligon-exampleapp-js`)
  .enablePlugins(ScalaJSPlugin)
  .settings(libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2")
  .settings(libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.7")
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val `poligon-vaadin` = project.dependsOn(`poligon-exampleapp` % "compile->compile;test->test")
  .settings(libraryDependencies ++= basicDeps.value)
  .settings(libraryDependencies += "com.vaadin" % "vaadin-server" % "7.7.15")
  .settings(libraryDependencies += "com.vaadin" % "vaadin-client-compiled" % "7.7.15")
  .settings(libraryDependencies += "com.vaadin" % "vaadin-themes" % "7.7.15")
  .settings(libraryDependencies += "com.vaadin" % "vaadin-push" % "7.7.15")
  .settings(libraryDependencies += "org.eclipse.jetty" % "jetty-server" % "9.4.12.v20180830")
  .settings(libraryDependencies += "org.eclipse.jetty" % "jetty-servlet" % "9.4.12.v20180830")
  .settings(libraryDependencies += "org.eclipse.jetty" % "jetty-continuation" % "9.4.12.v20180830")


lazy val `poligon-workout` = project.dependsOn(`poligon-macros` % "compile->compile;test->test")

lazy val poligon = project.in(file("."))
  .aggregate(`poligon-macros`, `poligon-scalaConfig`, `poligon-functional`)

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



lazy val `poligon-macros` = project
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    libraryDependencies += "com.avsystem.commons" %% "commons-macros" % "1.29.0",
    libraryDependencies += "com.avsystem.commons" %% "commons-core" % "1.29.0",
    libraryDependencies += "com.avsystem.commons" %% "commons-spring" % "1.29.0",
    libraryDependencies += "com.avsystem.commons" %% "commons-mongo" % "1.29.0"
  )

lazy val `poligon-scalaConfig` = project.dependsOn(`poligon-macros`)
  .settings(libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % Test)

lazy val `poligon-functional` = project.dependsOn(`poligon-macros`)
  .settings(libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.4.1")
  .settings(libraryDependencies += "org.mongodb.scala" %% "mongo-scala-driver" % "2.4.2")
  .settings(libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3")
  .settings(libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0")
  .settings(libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % Test)

lazy val `poligon-vaadin` = project.dependsOn(`poligon-macros`)
  .settings(libraryDependencies += "com.vaadin" % "vaadin-server" % "7.7.15")
  .settings(libraryDependencies += "com.vaadin" % "vaadin-client-compiled" % "7.7.15")
  .settings(libraryDependencies += "com.vaadin" % "vaadin-themes" % "7.7.15")
  .settings(libraryDependencies += "com.vaadin" % "vaadin-push" % "7.7.15")
  .settings(libraryDependencies += "org.eclipse.jetty" % "jetty-server" % "9.4.12.v20180830")
  .settings(libraryDependencies += "org.eclipse.jetty" % "jetty-servlet" % "9.4.12.v20180830")
  .settings(libraryDependencies += "org.eclipse.jetty" % "jetty-continuation" % "9.4.12.v20180830")
  .settings(libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3")
  .settings(libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0")
  .settings(libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % Test)


lazy val poligon = project.in(file("."))
  .aggregate(`poligon-macros`, `poligon-scalaConfig`, `poligon-functional`)
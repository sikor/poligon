
inThisBuild(Seq(
  scalaVersion := "2.12.1",
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
  autoAPIMappings := true
))

lazy val `poligon-macros` = project
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    libraryDependencies += "com.avsystem.commons" %% "commons-macros" % "1.20.4",
    libraryDependencies += "com.avsystem.commons" %% "commons-shared" % "1.20.4",
    libraryDependencies += "com.avsystem.commons" %% "commons-spring" % "1.20.4"
  )

lazy val `poligon-scalaConfig` = project.dependsOn(`poligon-macros`)
  .settings(libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % Test)

lazy val `poligon-functional` = project.dependsOn(`poligon-macros`)
  .settings(libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % Test)

lazy val poligon = project.in(file("."))
  .aggregate(`poligon-macros`, `poligon-scalaConfig`, `poligon-functional`)
  .settings(
    publishArtifact := false,
    publish := (),
    publishLocal := (),
    publishM2 := (),
    doc := (target in doc).value)
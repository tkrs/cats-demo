name := "cats-tut"
version := "1.0"
scalaVersion := "2.12.8"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.1")

fork in run := true

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.6.0",
  "org.typelevel" %% "cats-free" % "1.6.0"
).map(_.withSources)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Ypartial-unification",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-unused:_",
  "-Ywarn-numeric-widen",
  "-Ydelambdafy:method",
  "-Xfuture",
  "-Xlint"
)

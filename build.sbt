name := "cats-tut"
version := "1.0"
scalaVersion := "2.12.2"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

fork in run := true

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.9.0"
).map(_.withSources)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-unused-import",
  "-Ywarn-numeric-widen",
  "-Ydelambdafy:method",
  "-opt:l:classpath",
  "-Xfuture",
  "-Xlint"
)

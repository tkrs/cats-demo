name := "cats-demo"
version := "1.0"
scalaVersion := "2.12.0"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

fork in run := true

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.8.1",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.scala-lang.modules" %% "scala-java8-compat" % "0.8.0"
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
  "-Xfuture",
  "-Xlint"
)
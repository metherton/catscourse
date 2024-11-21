name := "cats"

version := "0.1"

scalaVersion := "2.13.3"

val catsVersion = "2.1.1"
val catsEffectVersion = "3.5.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  "com.typesafe.slick" %% "slick" % "3.3.3",
  "org.postgresql" % "postgresql" % "42.3.4",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.3.3",
  "com.github.tminglei" %% "slick-pg" % "0.20.3",
  "com.github.tminglei" %% "slick-pg_play-json" % "0.20.3",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.scalatest" %% "scalatest-funsuite" % "3.2.19" % "test",
  "org.scalatest" %% "scalatest-flatspec" % "3.2.19" % "test",
  "org.scalatest" %% "scalatest-funspec" % "3.2.19" % "test"
)



scalacOptions ++= Seq(
  "-language:higherKinds"
)
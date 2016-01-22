name := "SortableChallenge"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

scalacOptions ++= Seq("-feature", "-language:implicitConversions")

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.3.4"
)
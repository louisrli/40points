lazy val root = (project in file(".")).
  settings(
    name := "40 Points"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"
libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.1.0"
libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.1.0"


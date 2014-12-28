lazy val root = (project in file(".")).
  settings(
    name := "40 Points"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"


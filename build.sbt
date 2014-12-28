lazy val root = (project in file(".")).
  settings(
    name := "40 Points"
  )

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

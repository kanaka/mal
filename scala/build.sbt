lazy val root = (project in file(".")).
  settings(assemblySettings: _*).
  settings(
    name := "mal",
    version := "0.1",
    scalaVersion := "2.11.4"
  )

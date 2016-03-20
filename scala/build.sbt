lazy val root = (project in file(".")).
  settings(
    name := "mal",
    version := "0.1",
    scalaVersion := "2.11.4"
  )

// Suppress message for command line execution

onLoadMessage := ""

showSuccess := false

logLevel in runMain := Level.Warn

mainClass in Compile := Some("stepA_mal")

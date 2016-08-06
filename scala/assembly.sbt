import AssemblyKeys._ // put this at the top of the file

assemblySettings

test in assembly := {}
jarName in assembly := "mal.jar"
mainClass in assembly := Some("stepA_mal")
assemblyOption in assembly ~= { _.copy(prependShellScript = Some(defaultShellScript)) }

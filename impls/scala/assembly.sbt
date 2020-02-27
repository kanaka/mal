import sbtassembly.AssemblyPlugin.defaultShellScript

test in assembly := {}
assemblyJarName in assembly := "mal.jar"
mainClass in assembly := Some("stepA_mal")
assemblyOption in assembly ~= { _.copy(prependShellScript = Some(defaultShellScript)) }

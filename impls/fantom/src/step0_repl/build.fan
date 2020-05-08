class Build : build::BuildPod
{
  new make()
  {
    podName = "step0_repl"
    summary = "mal step0_repl pod"
    depends = ["sys 1.0"]
    srcDirs = [`fan/`]
    outPodDir = `lib/fan/`
  }
}

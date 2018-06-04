class Build : build::BuildPod
{
  new make()
  {
    podName = "step7_quote"
    summary = "mal step7_quote pod"
    depends = ["sys 1.0", "mallib 1.0"]
    srcDirs = [`fan/`]
    outPodDir = `lib/fan/`
  }
}

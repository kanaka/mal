class Build : build::BuildPod
{
  new make()
  {
    podName = "step5_tco"
    summary = "mal step5_tco pod"
    depends = ["sys 1.0", "mallib 1.0"]
    srcDirs = [`fan/`]
    outPodDir = `lib/fan/`
  }
}

class Build : build::BuildPod
{
  new make()
  {
    podName = "step8_macros"
    summary = "mal step8_macros pod"
    depends = ["sys 1.0", "mallib 1.0"]
    srcDirs = [`fan/`]
    outPodDir = `lib/fan/`
  }
}

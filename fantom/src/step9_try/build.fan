class Build : build::BuildPod
{
  new make()
  {
    podName = "step9_try"
    summary = "mal step9_try pod"
    depends = ["sys 1.0", "mallib 1.0"]
    srcDirs = [`fan/`]
    outPodDir = `lib/fan/`
  }
}

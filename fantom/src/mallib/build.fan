class Build : build::BuildPod
{
  new make()
  {
    podName = "mallib"
    summary = "mal library pod"
    depends = ["sys 1.0", "compiler 1.0"]
    srcDirs = [`fan/`]
    outPodDir = `lib/fan/`
  }
}

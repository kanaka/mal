class Build : build::BuildPod
{
  new make()
  {
    podName = "stepA_mal"
    summary = "mal stepA_mal pod"
    depends = ["sys 1.0", "mallib 1.0"]
    srcDirs = [`fan/`]
    outPodDir = `lib/fan/`
  }
}

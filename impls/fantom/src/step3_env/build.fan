class Build : build::BuildPod
{
  new make()
  {
    podName = "step3_env"
    summary = "mal step3_env pod"
    depends = ["sys 1.0", "mallib 1.0"]
    srcDirs = [`fan/`]
    outPodDir = `lib/fan/`
  }
}

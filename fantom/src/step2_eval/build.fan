class Build : build::BuildPod
{
  new make()
  {
    podName = "step2_eval"
    summary = "mal step2_eval pod"
    depends = ["sys 1.0", "mallib 1.0"]
    srcDirs = [`fan/`]
    outPodDir = `lib/fan/`
  }
}

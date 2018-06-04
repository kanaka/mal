class Build : build::BuildPod
{
  new make()
  {
    podName = "step4_if_fn_do"
    summary = "mal step4_if_fn_do pod"
    depends = ["sys 1.0", "mallib 1.0"]
    srcDirs = [`fan/`]
    outPodDir = `lib/fan/`
  }
}

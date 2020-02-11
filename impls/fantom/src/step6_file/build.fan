class Build : build::BuildPod
{
  new make()
  {
    podName = "step6_file"
    summary = "mal step6_file pod"
    depends = ["sys 1.0", "mallib 1.0"]
    srcDirs = [`fan/`]
    outPodDir = `lib/fan/`
  }
}

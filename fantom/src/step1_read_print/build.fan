class Build : build::BuildPod
{
  new make()
  {
    podName = "step1_read_print"
    summary = "mal step1_read_print pod"
    depends = ["sys 1.0", "mallib 1.0"]
    srcDirs = [`fan/`]
    outPodDir = `lib/fan/`
  }
}

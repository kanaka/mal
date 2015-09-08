import rdstdin, types, reader, printer

proc read(str: string): MalType = str.read_str

proc eval(ast: MalType): MalType = ast

proc print(exp: MalType): string = exp.pr_str

while true:
  try:
    let line = readLineFromStdin("user> ")
    echo line.read.eval.print
  except:
    echo getCurrentExceptionMsg()

import rdstdin

proc read(str: string): string = str

proc eval(ast: string): string = ast

proc print(exp: string): string = exp

while true:
 try:
  let line = readLineFromStdin("user> ")
  echo line.read.eval.print
 except IOError: quit()

import std/strutils
import bracketians

var stack: Stack
stack.add Layer()

while true:
  stdout.write ">> "
  stdout.flushFile

  let input = stdin.readLine.strip

  if input == "exit":
    echo "Good bye!"

  elif input == "":
    echo ""
    
  else:
    # echo parse input
    inspect repl(parse input, stack, defaultFunctionMap, defaultMacroMap)

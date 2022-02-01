import std/[strutils]
import bracketians

# echo parse(readFile "./examples/eg1.nim").join "\n"

echo parse(readFile "./examples/eg2.nim")[0].eval(defaultFunctionMap, defaultMacroMap)

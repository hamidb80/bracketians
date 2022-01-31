import std/[strutils]
import bracketians

# echo parse(readFile "./examples/eg1.nim").join "\n"

var s: Stack
echo parse(readFile "./examples/eg2.nim")[0].eval(s, defaultFunctionMap)

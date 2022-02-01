import std/[strutils]
import bracketians

# echo parse(readFile "./examples/eg1.nim").join "\n"

# echo parse(readFile "./examples/eg2.nim")[0].eval()
# echo parse(readFile "./examples/eg3.nim")[0].eval()
echo parse(readFile "./examples/eg4.nim")[0].eval()

import std/[strutils]
import bracketians

echo parse(readFile "./play.nim").join "\n"

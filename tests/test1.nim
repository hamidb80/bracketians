import std/[strutils]
import bracketians

var pppp: IRMap
echo parse(readFile "./play.nim", pppp).join "\n"

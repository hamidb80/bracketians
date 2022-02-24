import std/[tables, strformat, sequtils, strutils, os]
import bracketians


var fm = defaultFunctionMap

# wrting -----------------------------

func bBold(text: BNode{bnString}): BNode{bnString} {.bfKindAssersion, infer.} =
  toBNode fmt"<b>{text}</b>"

func bItalic(text: BNode{bnString}): BNode{bnString} {.bfKindAssersion, infer.} =
  toBNode fmt"<i>{text}</i>"

func bUnderLine(text: BNode{bnString}): BNode{bnString} {.bfKindAssersion, infer.} =
  toBNode fmt"<u>{text}</u>"

func bLink(text, link: BNode{bnString}): BNode{bnString} {.bfKindAssersion, infer.} =
  toBNode fmt"<a href={link}>{text}</a>"

func bCode(text, link: BNode{bnString}): BNode{bnString} {.bfKindAssersion, infer.} =
  toBNode fmt"<code>{text}</code>"

func bAsset(path: BNode{bnString}): BNode{bnString} {.bfKindAssersion, infer.} =
  let (_, _, ext) = splitFile $path

  toBNode:
    case ext.toLower:

    of ".mp4", ".webm":
      fmt "<video src=\"{path}\"></video>"

    of ".gif", ".png", "jpg", "jpeg":
      fmt "<img src=\"{path}\">"

    else:
      raise newException(ValueError, "the asset is not supported; extention:" & ext)

func bOrderedList(list: varargs[BNode]): BNode{bnString} {.bfKindAssersion, infer.} =
  toBNode:
    "<ui>" &
    list.mapIt(fmt"<ol>{it}</ol>").join &
    "</ui>"

func bList(list: varargs[BNode]): BNode{bnString} {.bfKindAssersion, infer.} =
  toBNode:
    "<ui>" &
    list.mapIt(fmt"<li>{it}</li>").join &
    "</ui>"

# book -----------------------------

type Chapter = tuple
  title: string
  content: string

var
  chapters: seq[Chapter]
  refs: seq[string]


proc defChapter(title: BNode{bnString}, body: varargs[BNode]): BNode{bnNothing}
  {.bfKindAssersion, infer.} =

  chapters.add ($title, body.mapIt($it).join)
  newBNothing()

proc bMakeRef(refName, content: BNode{bnString}): BNode{bnString}
  {.bfKindAssersion, infer.} =

  {.cast(noSideEffect).}:
    refs.add $refname

  toBNode fmt"""
        <span id="{refname}">{content}</span>
    """

proc bResolveRef(id, content: BNode{bnString}): BNode{bnString}
  {.bfKindAssersion, infer.} =

  {.cast(noSideEffect).}:
    assert $id in refs, "no such refrence found"

  toBNode fmt"""
    <a href="#{id}">{content}</a>
  """

# build ------------------------------------

proc genBook(chapters: seq[Chapter]): string =
  for i, ch in chapters:
    result &= fmt"""
      <div id="ch-{i+1}">{ch.content}</div>
    """

# register ---------------------------------

fm["b"] = bBold
fm["i"] = bItalic
fm["u"] = bUnderLine
fm["code"] = bCode
fm["link"] = bLink
fm["asset"] = bAsset
fm["orderedList"] = bOrderedList
fm["list"] = bList

fm["ch"] = defChapter
fm["ref"] = bMakeRef
fm["addr"] = bResolveRef

# go ---------------------------------------

inspect parse(readFile "./examples/bk1.bm").repl(fm)
writeFile "./temp/play.html", genBook chapters

import std/[tables, sequtils, strformat, strutils, macros]
import macroplus
import parser


type
    BracketianNodeKinds = enum
        bnNothing, bnBool, bnInt, bnFloat, bnString
        bnList, bnTable,
        # TODO bnLambda

    BracketianNode* = ref object
        case kind*: BracketianNodeKinds:
        of bnNothing: discard
        of bnBool: boolVal*: bool
        of bnInt: intVal*: int
        of bnFloat: floatVal*: float
        of bnString: strVal*: string
        of bnList: data*: seq[BracketianNode]
        of bnTable: table*: Table[BracketianNode, BracketianNode]

    BNode* = BracketianNode

    BracketianFn* =
        proc(args: seq[BracketianNode]): BracketianNode {.nimcall.}

    Symbol* = string

    Layer* = Table[Symbol, BracketianNode]
    Stack* = seq[Layer]

    FnMap* = Table[Symbol, BracketianFn]


func `$`*(n: BNode): string =
    case n.kind:
    of bnNothing:
        "nil"

    of bnBool:
        $n.boolVal

    of bnInt:
        $n.intVal

    of bnFloat:
        $n.floatVal

    of bnString:
        n.strVal

    of bnList:
        substr $n.data, 1

    of bnTable:
        '{' & n.table.pairs.toseq.mapIt(fmt"{it[0]}: {it[1]}").join(" ") & '}'

# ----------------------------

# TODO add kind assertion for return type
macro bfKindAssersion*(routine) =
    ## func gt(x, y: BNode{bInt}, z: Bnode{bBool}): BNode{bBool} =
    ##   ...
    ##
    ## is converted to =>
    ##
    ## func gt(x, y: BNode; z: Bnode): BNode =
    ##   assert z.kind == bBool
    ##   assert x.kind == bInt
    ##   assert y.kind == bInt
    ##   ...

    let rf = routine[RoutineFormalParams]

    for i, param in rf.pairs:
        if i == 0: # return type
            rf[i] = param[0]

        elif param[IdentDefType].kind == nnkCurlyExpr: # args
            let desiredKind = param[IdentDefType][1]
            rf[i][IdentDefType] = param[IdentDefType][0]

            routine[RoutineBody].insert 0, toStmtList do:
                rf[i][IdentDefNames].mapIt quote do:
                    assert `it`.kind == `desiredKind`

    return routine

macro infer(routine) =
    ## proc job(a, b: bool, c: bool): bool =
    ##    discard
    ##
    ## converts to =>
    ##
    ##  proc job(args: seq[bool): bool =
    ##    assert args.len == 3
    ##    let
    ##      a = args[0]
    ##      b = args[1]
    ##      c = args[2]
    ##    discard

    var hasVarargs = false # TODO support varargs
    let
        rt = routine.RoutineReturnType
        argsI = "args".ident
        identArgs = block:
            var res: seq[NimNode]

            for identDef in routine.RoutineArguments:
                res.add identDef[IdentDefNames]

                let idt = identDef[IdentDefType]
                if idt.kind == nnkBracketExpr and idt.strVal == "varargs":
                    hasVarargs = true

            res


    routine[RoutineFormalParams] = newTree(nnkFormalParams,
        rt, newIdentDefs(argsI, inlineQuote seq[`rt`]))

    var acc = 0
    template spin: untyped =
        let res = acc
        acc.inc
        res

    routine[RoutineBody].insert 0, newNimNode(nnkLetSection).add do:
        identArgs.mapit newIdentDefs(it, newEmptyNode(), block:
            let i = spin()
            inlineQuote `argsI`[`i`]
        )

    routine[RoutineBody].insert 0, quote do:
        assert `argsI`.len == `acc`

    # echo repr routine
    return routine


func toBNode*(i: int): BNode =
    BNode(kind: bnInt, intVal: i)

func toBNode*(f: float): BNode =
    BNode(kind: bnFloat, floatVal: f)

func toBNode*(s: string): BNode =
    BNode(kind: bnString, strVal: s)

func toBNode*(b: bool): BNode =
    BNode(kind: bnBool, boolVal: b)

func newBNothing*(): BNode =
    BNode(kind: bnNothing)

func bLen(s: BNode{bnString}): BNode{bnInt} {.bfKindAssersion, infer.} =
    toBNode s.strVal.len

proc bEcho(bn: BNode): BNode {.infer.} =
    {.cast(nosideEffect).}:
        echo bn

    newBNothing()

let defaultFunctionMap*: FnMap = toTable {
    "len": bLen,
    "echo": bEcho
}

# TODO add macroMap for if def cond ...
proc eval*(tk: BToken, stack: Stack, fm: FnMap): BNode =
    case tk.kind:
    of btNothing: BNode(kind: bnNothing)
    of btInt: toBNode(tk.intval)
    of btFloat: toBNode(tk.floatVal)
    of btString: toBNode(tk.strVal)
    of btBool: toBNode(tk.boolVal)

    of btSymbol:
        for i in countdown(stack.high, 0):
            if tk.symbol in stack[i]:
                return stack[i][tk.symbol]

        raise newException(ValueError, fmt"the symbol '{tk.symbol}' is not defined")

    of btList:
        doAssert tk.data.len != 0, "a list cannot have 0 elements"
        doAssert tk.data[0].kind == btSymbol

        fm[tk.data[0].symbol](tk.data[1..^1].mapIt eval(it, stack, fm))

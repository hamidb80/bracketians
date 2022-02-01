import std/[tables, sequtils, strformat, strutils, macros]
import macroplus
import parser


type
    BracketianNodeKinds = enum
        bnNothing, bnBool, bnInt, bnFloat, bnString
        bnList, bnTable, bnLambda

    Symbol* = string

    BracketianNode* = ref object
        case kind*: BracketianNodeKinds:
        of bnNothing: discard
        of bnBool: boolVal*: bool
        of bnInt: intVal*: int
        of bnFloat: floatVal*: float
        of bnString: strVal*: string
        of bnList: data*: seq[BracketianNode]
        of bnTable: table*: Table[BracketianNode, BracketianNode]
        of bnLambda:
            args*: seq[Symbol]
            instructions*: seq[BToken]

    BracketianFn* =
        proc(args: seq[BracketianNode]): BracketianNode {.nimcall.}

    BNode* = BracketianNode


    Layer* = TableRef[Symbol, BNode]
    Stack* = seq[Layer]

    FnMap* = Table[Symbol, BracketianFn]

    BracketianMacro* =
        proc(args: seq[BToken]): BToken {.nimcall.}

    MacroMap* = Table[Symbol, BracketianMacro]


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

    of bnLambda:
        "[lambda [" & n.args.join(" ") & "] " & n.instructions.join(" ") & " ]"

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

func newBList*(): BNode =
    BNode(kind: bnList)

func isTrue*(n: BNode): bool =
    assert n.kind == bnBool
    n.boolVal

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

macro infer*(routine) =
    ## + implicit return
    ## + varargs support
    ##
    ## proc job(a, b: BNode, c: varargs[BNode]) =
    ##    discard
    ##
    ## converts to =>
    ##
    ##  proc job(args: seq[BNode): BNode =
    ##    result = newBNothing()
    ##    assert args.len == 3, ""
    ##    let
    ##      a = args[0]
    ##      b = args[1]
    ##      c = args[2 .. ^1]
    ##    discard

    var
        hasReturn, hasVarargs = false
        defines = newNimNode(nnkLetSection)

    let
        argsI = "args".ident

        rt = # return type
            if routine.RoutineReturnType.kind == nnkEmpty:
                inlineQuote `BNode`
            else:
                hasReturn = true
                routine.RoutineReturnType

        identArgs = block:
            var res: seq[NimNode]

            for identDef in routine.RoutineArguments:
                res.add identDef[IdentDefNames]

                let idt = identDef[IdentDefType]
                if idt.kind == nnkBracketExpr and idt[0].strVal == "varargs":
                    hasVarargs = true

            res

        argslen = identArgs.len


    routine[RoutineFormalParams] = newTree(nnkFormalParams,
        rt, newIdentDefs(argsI, inlineQuote seq[`rt`]))

    for i, ia in identArgs:
        defines.add newIdentDefs(ia, newEmptyNode(),
            if i == identArgs.high and hasVarargs:
                inlineQuote `argsI`[`i` .. ^1]
            else:
                inlineQuote `argsI`[`i`]
        )

    routine[RoutineBody].insert 0, defines
    routine[RoutineBody].insert 0:
        if hasVarargs:
            inlineQuote assert(`argsI`.len >= (`argslen` - 1),
              "expected at least" & $(`argslen` - 1) & " but given " &
                      $`argsI`.len & " arguments")
        else:
            inlineQuote assert(`argsI`.len == `argslen`,
              "expected " & $`argslen` & " but given " & $`argsI`.len & " arguments")

    # implicit return type
    if not hasReturn:
        routine[RoutineBody].insert 0, quote do:
            result = `newBNothing`()

    # echo repr routine
    return routine

# --------------------------

func bLen(s: BNode{bnString}): BNode{bnInt} {.bfKindAssersion, infer.} =
    toBNode s.strVal.len

proc bEcho(bn: BNode): BNode {.infer.} =
    {.cast(nosideEffect).}:
        echo bn

    newBNothing()

func bCond(bns: varargs[BNode]): BNode {.infer.} =
    assert bns.len mod 2 == 0

    for i in countup(0, bns.high, 2):
        if bns[i].isTrue:
            return bns[i+1]

    newBNothing()

func defLambda(nodes: seq[BToken]): BNode =
    assert nodes.len >= 2

    let
        argsList = nodes[0]
        body = nodes[1..^1]

    assert:
        argsList.kind == btList and
        argsList.data.allit(it.kind == btSymbol)

    BNode(kind: bnLambda,
        args: argsList.data.mapIt(it.symbol),
        instructions: body)

func bToList(bns: varargs[BNode]): BNode {.infer.} =
    result = newBList()
    result.data.add bns

func bAdd(a, b: BNode): BNode {.infer.} =
    assert a.kind == b.kind

    case a.kind:
    of bnInt: toBNode(a.intVal + b.intVal)
    of bnFloat: toBNode(a.floatVal + b.floatVal)
    else:
        raise newException(ValueError, "not a number")

func ifStmt(args: seq[BToken]): BToken =
    BToken(kind: btNothing)

let
    defaultFunctionMap*: FnMap = toTable {
        "len": bLen,
        "echo": bEcho,
        "cond": bCond,
        "!": bToList,
        "+": bAdd,
    }

    defaultMacroMap*: MacroMap = toTable {
        "if": ifStmt
    }

# --------------------------
proc repl*(tks: seq[BToken], stack: var Stack, fm: FnMap, mm: MacroMap): BNode

proc eval*(tk: BToken, stack: var Stack, fm: FnMap, mm: MacroMap): BNode =
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
        doAssert tk.data[0].kind == btSymbol, "the first argument of a list must be a symbol"

        let
            callName = tk.data[0].symbol
            args = tk.data[1..^1]

        case callName:
        of "lambda", "fn":
            defLambda(args)

        of "call", "()": # lambda call
            assert args.len >= 1

            let
                fn = eval(args[0], stack, fm, mm)
                params =
                    if args.len == 2: eval(args[1], stack, fm, mm)
                    else: newBList()

                newStackLayer = block:
                    var l = Layer()

                    assert:
                        fn.kind == bnLambda and
                        fn.args.len == params.data.len

                    for i in 0 .. fn.args.high:
                        l[fn.args[i]] = params.data[i]

                    l

            stack.add newStackLayer
            let res = repl(fn.instructions, stack, fm, mm)
            stack.del stack.high

            res

        elif callName in mm:
            eval(mm[callName](args), stack, fm, mm)

        elif callName in fm:
            fm[callName](args.mapIt eval(it, stack, fm, mm))

        else:
            raise newException(ValueError,
                    "no such macro or function found with name: " & callName)

proc repl*(tks: seq[BToken], stack: var Stack, fm: FnMap, mm: MacroMap): BNode =
    for tk in tks:
        result = eval(tk, stack, fm, mm)

proc eval*(tk: BToken, fm: FnMap, mm: MacroMap): BNode =
    var s: Stack
    s.add Layer()
    eval(tk, s, fm, mm)

proc eval*(tk: BToken): BNode =
    eval(tk, defaultFunctionMap, defaultMacroMap)

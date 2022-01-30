import std/[strutils, strformat, sequtils, tables]


type
    BracketianNodeKind = enum
        bnNothing
        bnInt
        bnFloat
        bnBool
        bnString
        bnSymbol
        bnData
        bnCall

    BracketianNode = ref object
        case kind: BracketianNodeKind
        of bnNothing: discard
        of bnInt: intVal: int
        of bnFloat: floatVal: float
        of bnString: text: string
        of bnSymbol: symbol: string
        of bnBool: boolVal: bool
        of bnData:
            data: seq[BracketianNode]
        of bnCall:
            fn: string
            args: seq[BracketianNode]

    BnParser = proc(
        calledBy: string, nodes: seq[BracketianNode]): BracketianNode {.nimcall.}

    # IR :: intermidiate representation
    IRMap = Table[string, BnParser]

    ParserStates = enum
        psInitial
        psNumber
        psString
        psSymbol


func `$`(node: BracketianNode): string =
    case node.kind:
    of bnNothing:
        "nil"

    of bnBool:
        $node.boolVal

    of bnFloat:
        $node.floatVal

    of bnInt:
        $node.intVal

    of bnString:
        '"' & node.text & '"'

    of bnSymbol:
        node.symbol

    of bnData:
        '(' & node.data.join(" ") & ')'

    of bnCall:
        '['& node.fn & ' ' & node.args.join(" ") & ']'

func add(b: var BracketianNode, n: BracketianNode) =
    doassert b.kind == bnData, $b.kind
    b.data.add n

proc parse*(
    s: ref string, pm: IRMap,
    startI: int, acc: var BracketianNode
): int =
    ## return the last index that was there

    var
        state: ParserStates = psInitial
        i = startI
        temp = 0

    template reset: untyped = state = psInitial
    template done: untyped = return i
    template checkDone: untyped =
        if c == ']':
            return i

    while i < s[].len:
        let c = s[i]
        echo state

        case state:
        of psString:
            if c == '"' and s[i-1] != '\\':
                acc.add BracketianNode(kind: bnString, text: s[temp .. i-1])
                reset()

        of psSymbol:
            if c in Whitespace or c == ']':
                acc.add BracketianNode(kind: bnSymbol, symbol: s[temp .. i-1])

                reset()
                checkDone()

        of psNumber:
            if c in Whitespace or c == ']':
                let t = s[temp .. i-1]

                acc.add:
                    if '.' in t:
                        BracketianNode(kind: bnFloat, floatVal: parseFloat t)
                    else:
                        BracketianNode(kind: bnInt, intval: parseInt t)

            reset()
            checkDone()

        of psInitial:
            case c:
            of '[':
                var node = BracketianNode(kind: bnData)
                echo ">>"
                i = parse(s, pm, i+1, node)
                echo "<<"
                acc.add BracketianNode(kind: bnCall, fn: node.data[0].symbol, args: node.data[1..^1])

            of ']':
                done()

            of Whitespace:
                discard

            of {'0' .. '9', '.'}:
                state = psNumber
                temp = i
            
            of '"':
                state = psString
                temp = i+1

            else:
                state = psSymbol
                temp = i

        echo (i, c)
        i.inc

proc parse*(s: ref string, pm: IRMap): BracketianNode =
    result = BracketianNode(kind: bnData)
    discard parse(s, pm, 0, result)

proc parse*(s: string, pm: IRMap): BracketianNode =
    let sref = new string
    sref[] = s
    parse(sref, pm)

var pppp: IRMap

echo parse(readFile "./eg1.nim", pppp)
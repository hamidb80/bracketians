import std/[strutils, sequtils, tables]


type
    BracketianNodeKind* = enum
        bnNothing
        bnInt
        bnFloat
        bnBool
        bnString
        bnSymbol
        bnList
        bnCall
        bnTable

    BracketianNode* = ref object
        case kind: BracketianNodeKind
        of bnNothing: discard
        of bnInt: intVal*: int
        of bnFloat: floatVal*: float
        of bnString: strVal*: string
        of bnSymbol: symbol*: string
        of bnBool: boolVal*: bool
        of bnList:
            list*: seq[BracketianNode]
        of bnTable:
            table*: Table[BracketianNode, BracketianNode]
        of bnCall:
            fn*: string
            args*: seq[BracketianNode]

    BnParser* = proc(
        calledBy: string, nodes: seq[BracketianNode]): BracketianNode {.nimcall.}

    # IR :: intermidiate representation
    IRMap* = Table[string, BnParser]

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
        '"' & node.strVal & '"'

    of bnSymbol:
        node.symbol

    of bnList:
        '(' & node.list.join(" ") & ')'

    of bnTable:
        '{' & node.table.pairs.toseq.mapIt($it[0] & ':' & $it[1]).join(" ") & '}'

    of bnCall:
        '[' & node.fn & ' ' & node.args.join(" ") & ']'

proc parse*(
    s: ref string, pm: IRMap,
    startI: int, acc: var seq[BracketianNode]
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
        # echo state

        case state:
        of psString:
            if c == '"' and s[i-1] != '\\':
                acc.add BracketianNode(kind: bnString, strVal: s[temp .. i-1])
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
                var nodes: seq[BracketianNode]
                i = parse(s, pm, i+1, nodes)

                acc.add BracketianNode(kind: bnCall, fn: nodes[0].symbol,
                        args: nodes[1..^1])

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

        # echo (i, c)
        i.inc

proc parse*(s: ref string, pm: IRMap): seq[BracketianNode] =
    discard parse(s, pm, 0, result)

proc parse*(s: string, pm: IRMap): seq[BracketianNode] =
    let sref = new string
    sref[] = s
    parse(sref, pm)


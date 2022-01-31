import std/[strutils, tables]


type
    BracketianTokenKind* = enum
        bnNothing
        bnBool, bnInt, bnFloat, bnString
        bnSymbol
        bnList

    BracketianToken* = ref object
        case kind: BracketianTokenKind
        of bnNothing: discard
        of bnInt: intVal*: int
        of bnFloat: floatVal*: float
        of bnString: strVal*: string
        of bnSymbol: symbol*: string
        of bnBool: boolVal*: bool
        of bnList: data*: seq[BracketianToken]

    BToken* = BracketianToken

    ParserStates = enum
        psInitial
        psNumber
        psString
        psSymbol


func `$`(node: BToken): string =
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
        '[' & node.data.join(" ") & ']'

proc parse*(
    s: ref string, startI: int, acc: var seq[BToken]
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
                acc.add BToken(kind: bnString, strVal: s[temp .. i-1])
                reset()

        of psSymbol:
            if c in Whitespace or c == ']':
                acc.add BToken(kind: bnSymbol, symbol: s[temp .. i-1])

                reset()
                checkDone()

        of psNumber:
            if c in Whitespace or c == ']':
                let t = s[temp .. i-1]

                acc.add:
                    if '.' in t:
                        BToken(kind: bnFloat, floatVal: parseFloat t)
                    else:
                        BToken(kind: bnInt, intval: parseInt t)

            reset()
            checkDone()

        of psInitial:
            case c:
            of '[':
                var nodes: seq[BToken]
                i = parse(s, i+1, nodes)

                acc.add BToken(kind: bnList, data: nodes)

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

proc parse*(s: ref string): seq[BToken] =
    discard parse(s, 0, result)

proc parse*(s: string): seq[BToken] =
    let sref = new string
    sref[] = s
    parse(sref)

# proc eval*():

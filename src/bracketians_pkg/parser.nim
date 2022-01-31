import std/[strutils]


type
    BracketianTokenKinds* = enum
        btNothing
        btBool, btInt, btFloat, btString
        btSymbol
        btList

    BracketianToken* = ref object
        case kind*: BracketianTokenKinds
        of btNothing: discard
        of btInt: intVal*: int
        of btFloat: floatVal*: float
        of btString: strVal*: string
        of btSymbol: symbol*: string
        of btBool: boolVal*: bool
        of btList: data*: seq[BracketianToken]

    BToken* = BracketianToken

    ParserStates = enum
        psInitial
        psNumber
        psString
        psSymbol

func `$`*(tk: BToken): string =
    case tk.kind:
    of btNothing:
        "nil"

    of btBool:
        $tk.boolVal

    of btFloat:
        $tk.floatVal

    of btInt:
        $tk.intVal

    of btString:
        '"' & tk.strVal & '"'

    of btSymbol:
        tk.symbol

    of btList:
        '[' & tk.data.join(" ") & ']'

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
                acc.add BToken(kind: btString, strVal: s[temp .. i-1])
                reset()

        of psSymbol:
            if c in Whitespace or c == ']':
                acc.add BToken(kind: btSymbol, symbol: s[temp .. i-1])

                reset()
                checkDone()

        of psNumber:
            if c in Whitespace or c == ']':
                let t = s[temp .. i-1]

                acc.add:
                    if '.' in t:
                        BToken(kind: btFloat, floatVal: parseFloat t)
                    else:
                        BToken(kind: btInt, intval: parseInt t)

            reset()
            checkDone()

        of psInitial:
            case c:
            of '[':
                var nodes: seq[BToken]
                i = parse(s, i+1, nodes)

                acc.add BToken(kind: btList, data: nodes)

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

proc parse*(s: string): seq[BToken] =
    let sref = new string
    sref[] = s
    discard parse(sref, 0, result)

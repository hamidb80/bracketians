import std/[strutils]


type
    BracketianTokenKinds* = enum
        btNothing
        btBool, btInt, btFloat, btString
        btSymbol
        btCall, btList

    BracketianToken* = ref object
        case kind*: BracketianTokenKinds
        of btNothing: discard
        of btInt: intVal*: int
        of btFloat: floatVal*: float
        of btString: strVal*: string
        of btSymbol: symbol*: string
        of btBool: boolVal*: bool
        of btList: data*: seq[BracketianToken]
        of btCall:
            caller*: string
            args*: seq[BracketianToken]

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
        '(' & tk.data.join(" ") & ')'

    of btCall:
        '[' & tk.caller & ' ' & tk.args.join(" ") & ']'


proc parse*(
    s: ref string, startI: int, acc: var seq[BToken]
): int =
    ## return the last index that was there

    const rrr = {']', ')'}

    var
        state: ParserStates = psInitial
        i = startI
        temp = 0

    template reset: untyped = state = psInitial
    template done: untyped = return i
    template checkDone: untyped =
        if c in rrr:
            return i

    while i < s[].len:
        let c = s[i]

        case state:
        of psString:
            if c == '"' and s[i-1] != '\\':
                acc.add BToken(kind: btString, strVal: s[temp .. i-1])
                reset()

        of psSymbol:
            if c in Whitespace or c in rrr:
                let t = s[temp .. i-1]

                acc.add:
                    if t in ["true", "false"]:
                        BToken(kind: btBool, boolVal: parseBool t)
                    else:
                        BToken(kind: btSymbol, symbol: t)


                reset()
                checkDone()

        of psNumber:
            if c in Whitespace or c in rrr:
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
            of '[', '(':
                var nodes: seq[BToken]
                i = parse(s, i+1, nodes)

                acc.add:
                    if c == '(':
                        BToken(kind: btList, data: nodes)
                    else:
                        assert nodes.len >= 1, "a call must have at least on element"
                        assert nodes[0].kind == btSymbol, "caller must be a symbol"

                        BToken(kind: btCall, caller: nodes[0].symbol,
                                args: nodes[1 .. ^1])

            of ']', ')':
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

        i.inc

proc parse*(s: string): seq[BToken] =
    let sref = new string
    sref[] = s
    discard parse(sref, 0, result)

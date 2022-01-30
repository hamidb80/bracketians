import std/[strutils, sequtils, tables]


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

proc parser*(
    s: ref string, pm: IRMap, 
    startI: int, acc: var seq[BracketianNode]
): int =
    ## return the last index that was there

    var
        state: ParserStates
        i = 0
        temp = 0

    while i < s[].len:
        let c = s[i]

        case state:
        of psString:
            if c == '"' and s[i-1] != '\\':
                acc.add BracketianNode(kind: bnString, text: s[temp .. i-1])

            state = psInitial

        of psSymbol:
            discard

        of psNumber:
            discard

        of psInitial:
            case c:
            of '[':
                var node: seq[BracketianNode]
                i = parser(s, pm, i+1, node)

            of ']':
                return i

            else:
                discard

        i.inc

    raise newException(ValueError, "no stopper")

proc parser*(s: ref string, pm: IRMap): seq[BracketianNode] =
    discard parser(s, pm, 0, result)

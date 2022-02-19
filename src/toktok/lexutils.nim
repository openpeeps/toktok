from std/strutils import Whitespace

proc init*[T: typedesc[Lexer]](lex: T; fileContents: string): Lexer =
    ## Initialize a new BaseLexer instance with given Stream
    var lex = Lexer()
    lexbase.open(lex, newStringStream(fileContents))
    lex.startPos = 0
    lex.kind = TK_UNKNOWN
    lex.token = ""
    lex.error = ""
    return lex

const NUMBERS = {'0'..'9'}
const AZaz = {'a'..'z', 'A'..'Z', '_', '-'}

template setError*[L: Lexer](lexer: var L; message: string): untyped =
    lexer.kind = TK_UNKNOWN
    if lexer.error.len == 0:
        lexer.error = message

proc hasError*[L: Lexer](lexer: L): bool {.inline.} =
    ## Determine if Lexer has any errors
    result = lexer.error.len != 0

proc getError*[L: Lexer](lexer: L): string {.inline.} =
    ## Retrieve error message from Lexer object
    result = lexer.error

proc handleNewLine[T: Lexer](lex: var T) =
    ## Handle new lines
    case lex.buf[lex.bufpos]
    of '\c': lex.bufpos = lex.handleCR(lex.bufpos)
    of '\n': lex.bufpos = lex.handleLF(lex.bufpos)
    else: discard

proc skip*[T: Lexer](lex: var T) =
    ## Procedure for skipping/offset between columns/positions 
    var wsno: int
    while true:
        case lex.buf[lex.bufpos]
        of Whitespace:
            if lex.buf[lex.bufpos] notin NewLines:
                inc lex.bufpos
                inc wsno
            else: lex.handleNewLine()
        else:
            lex.wsno = wsno
            break

proc existsInBuffer[T: Lexer](lex: var T, pos: int, chars:set[char]): bool = 
    lex.buf[pos] in chars

proc hasLetters[T: Lexer](lex: var T, pos: int): bool =
    lex.existsInBuffer(pos, AZaz)

proc hasNumbers[T: Lexer](lex: var T, pos: int): bool =
    lex.existsInBuffer(pos, NUMBERS)

template handleNumber*[T: Lexer](lex: var T) =
    lex.startPos = lex.getColNumber(lex.bufpos)
    lex.token = "0"
    while lex.buf[lex.bufpos] == '0':
        inc lex.bufpos
    while true:
        case lex.buf[lex.bufpos]
        of '0'..'9':
            if lex.token == "0":
                setLen(lex.token, 0)
            add lex.token, lex.buf[lex.bufpos]
            inc lex.bufpos
        of 'a'..'z', 'A'..'Z', '_':
            lex.setError("Invalid number")
            return
        else:
            lex.setToken(TK_INTEGER)
            break

template handleIdent*[T: Lexer](lex: var T) =
    ## Template to handle string-based identifiers
    lex.startPos = lex.getColNumber(lex.bufpos)
    setLen(lex.token, 0)
    while true:
        if lex.hasLetters(lex.bufpos):
            add lex.token, lex.buf[lex.bufpos]
            inc lex.bufpos
        elif lex.hasNumbers(lex.bufpos):
            add lex.token, lex.buf[lex.bufpos]
            inc lex.bufpos
        else: break

    skip lex
    lex.generateIdentCase()           # template defined in toktok

proc setToken*[T: Lexer](lexer: var T, tokenKind: TokenKind, offset = 1) =
    ## Set meta data for current token
    lexer.kind = tokenKind
    lexer.startPos = lexer.getColNumber(lexer.bufpos)
    inc(lexer.bufpos, offset)
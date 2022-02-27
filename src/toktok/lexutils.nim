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

const numbers = {'0'..'9'}
const azAZ = {'a'..'z', 'A'..'Z', '_', '-'}

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
    lex.existsInBuffer(pos, azAZ)

proc hasNumbers[T: Lexer](lex: var T, pos: int): bool =
    lex.existsInBuffer(pos, numbers)

template setTokenMulti*[T: Lexer](lex: var T, tokenKind: TokenKind, offset = 0, multichars = 0) =
    ## Set meta data of the current token and jump to the next one
    skip lex
    lex.startPos = lex.getColNumber(lex.bufpos)
    var items = 0
    if multichars != 0:
        while items < multichars:
            add lex.token, lex.buf[lex.bufpos]
            inc lex.bufpos
            inc items
    else:
        add lex.token, lex.buf[lex.bufpos]
        inc lex.bufpos, offset
    lex.kind = tokenKind

proc nextToEOL*[T: Lexer](lex: var T): tuple[pos: int, token: string] =
    ## Get entire buffer starting from given position to the end of line
    while true:
        case lex.buf[lex.bufpos]:
        of NewLines: return
        of EndOfFile: return
        else: 
            add lex.token, lex.buf[lex.bufpos]
            inc lex.bufpos
    return (pos: lex.bufpos, token: lex.token)

proc next*[T: Lexer](lex: var T, tkChar: char, offset = 1): bool =
    ## Determine if the next character is as expected,
    ## without modifying the current buffer position
    skip lex
    result = lex.buf[lex.bufpos + offset] in {tkChar}

proc next*[T: Lexer](lex: var T, chars:string): bool =
    ## Determine the next characters based on given chars string,
    ## without modifying the current buffer position
    var i = 1
    var status = false
    for c in chars.toSeq():
        status = lex.next(c, i)
        inc i
    result = status

proc setToken*[T: Lexer](lexer: var T, tokenKind: TokenKind, offset = 1) =
    ## Set meta data for current token
    lexer.kind = tokenKind
    lexer.startPos = lexer.getColNumber(lexer.bufpos)
    inc(lexer.bufpos, offset)

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
        else: break
    lex.kind = TK_INTEGER

proc handleSpecial[T: Lexer](lex: var T): char =
    ## Procedure for for handling special escaping tokens
    assert lex.buf[lex.bufpos] == '\\'
    inc lex.bufpos
    case lex.buf[lex.bufpos]
    of 'n':
        lex.token.add "\\n"
        result = '\n'
        inc lex.bufpos
    of '\\':
        lex.token.add "\\\\"
        result = '\\'
        inc lex.bufpos
    else:
        lex.setError("Unknown escape sequence: '\\" & lex.buf[lex.bufpos] & "'")
        result = '\0'

proc handleString[T: Lexer](lex: var T) =
    ## Handle string values wrapped in single or double quotes
    lex.startPos = lex.getColNumber(lex.bufpos)
    lex.token = ""
    inc lex.bufpos
    while true:
        case lex.buf[lex.bufpos]
        of '\\':
            discard lex.handleSpecial()
            if lex.hasError(): return
        of '"':
            lex.kind = TK_STRING
            inc lex.bufpos
            break
        of NewLines:
            lex.setError("EOL reached before end of string")
            return
        of EndOfFile:
            lex.setError("EOF reached before end of string")
            return
        else:
            add lex.token, lex.buf[lex.bufpos]
            inc lex.bufpos

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
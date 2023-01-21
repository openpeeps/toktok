# TokTok, Generic tokenizer written in Nim language. Powered by Nim's Macros.
#
# (c) 2021 TokTok is released under MIT License
#          George Lemon | Made by Humans from OpenPeep
#          https://github.com/openpeep/toktok

import std/strutils except NewLines

proc init*[T: typedesc[Lexer]](lex: T; fileContents: string, allowMultilineStrings = false): Lexer =
  ## Initialize a new BaseLexer instance with given Stream
  var lex = Lexer()
  open(lex, newStringStream(fileContents))
  lex.startPos = 0
  lex.kind = TK_UNKNOWN
  lex.token = ""
  lex.error = ""
  lex.multiLineStr = allowMultilineStrings
  result = lex

const numbers = {'0'..'9'}
const azAZ = {'a'..'z', 'A'..'Z', '_', '-'}

proc setError*(lex: var Lexer; message: string) =
  lex.kind = TK_UNKNOWN
  if lex.error.len == 0:
    lex.error = message

proc hasError*(lex: Lexer): bool {.inline.} =
  ## Determine if Lexer has any errors
  result = lex.error.len != 0

proc getError*(lex: Lexer): string {.inline.} =
  ## Retrieve error message from Lexer object
  result = "($1:$2) $3" % [$lex.lineNumber, $(lex.startPos), lex.error]

proc handleNewLine(lex: var Lexer) =
  ## Handle new lines
  case lex.buf[lex.bufpos]
  of '\c': lex.bufpos = lex.handleCR(lex.bufpos)
  of '\n': lex.bufpos = lex.handleLF(lex.bufpos)
  else: discard

proc inBuffer(lex: var Lexer, pos: int, chars:set[char]): bool =
  try:
    result = lex.buf[pos] in chars
  except IndexDefect:
    result = false

proc hasLetters*(lex: var Lexer, pos: int): bool =
  lex.inBuffer(pos, azAZ)

proc hasNumbers*(lex: var Lexer, pos: int): bool =
  lex.inBuffer(pos, numbers)

proc skip*(lex: var Lexer) =
  var wsno: int
  while true:
    case lex.buf[lex.bufpos]
    of Whitespace:
      if lex.buf[lex.bufpos] in NewLines:
        lex.handleNewLine()
      else:
        inc lex.bufpos
        inc wsno
    else:
      lex.wsno = wsno
      return

proc setTokenMulti(lex: var Lexer, tokenKind: TokenKind, offset = 0, multichars = 0) =
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

proc nextToEOL(lex: var Lexer, offset = 1): tuple[pos: int, token: string] =
  ## Get entire buffer starting from given position to the end of line
  inc lex.bufpos, offset
  let wsno = lex.wsno
  let col = lex.getColNumber(lex.bufpos)  # TODO keep initial start position
  skip lex
  while true:
    case lex.buf[lex.bufpos]:
    of NewLines: return
    of EndOfFile: return
    else: 
      add lex.token, lex.buf[lex.bufpos]
      inc lex.bufpos
  lex.wsno = wsno
  lex.startPos = col
  result = (pos: lex.bufpos, token: lex.token)

proc handleSpecial(lex: var Lexer) =
  ## Procedure for for handling special escaping tokens
  inc lex.bufpos
  case lex.buf[lex.bufpos]
  of 'n':
    add lex.token, "\\n"
    inc lex.bufpos
  of '\\':
    add lex.token, "\\\\"
    inc lex.bufpos
  of '"':
    add lex.token, "\\\""
    inc lex.bufpos
  else:
    lex.setError("Unknown escape sequence: '\\" & lex.buf[lex.bufpos] & "'")

proc next(lex: var Lexer, tkChar: char, offset = 1): bool =
  # Determine if next char is as expected
  # without modifying the current buffer
  skip lex
  result = lex.buf[lex.bufpos + offset] in {tkChar}

proc next(lex: var Lexer, chars:string): bool =
  # Determine if next group of chars is
  # as expected without modifying the current buffer
  var i = 1
  var status = false
  for c in chars:
    status = lex.next(c, i)
    if status == false:
      return status
    inc i
  result = status

proc nextToSpec(lex: var Lexer, endChar: char, tokenKind: TokenKind, str = "") =
  ## Handle string values wrapped in single or double quotes
  lex.startPos = lex.getColNumber(lex.bufpos)
  # lex.token = ""
  inc lex.bufpos
  while true:
    if lex.buf[lex.bufpos] == '\\':
      lex.handleSpecial()
      if lex.hasError(): return
    elif lex.buf[lex.bufpos] == endChar:
      lex.kind = tokenKind
      inc lex.bufpos
      break
    # elif lex.buf[lex.bufpos] in NewLines:
      # lex.handleNewLine()
      # lex.setError("EOL reached before end of input")
      # return
    elif lex.buf[lex.bufpos] == EndOfFile:
      lex.setError("EOF reached before end of input")
      return
    else:
      if str.len != 0:
        if lex.buf[lex.bufpos] == str[0]:
          inc lex.bufpos
          continue
      add lex.token, lex.buf[lex.bufpos]
      inc lex.bufpos

proc nextToSpec(lex: var Lexer, endChar: string, tokenKind: TokenKind) =
  lex.nextToSpec(endChar[^1], tokenKind, endChar)

proc setToken(lexer: var Lexer, tokenKind: TokenKind, offset = 1) =
  ## Set meta data for current token
  lexer.kind = tokenKind
  lexer.startPos = lexer.getColNumber(lexer.bufpos)
  inc(lexer.bufpos, offset)

proc handleNumber(lex: var Lexer) =
  lex.startPos = lex.getColNumber(lex.bufpos)
  var toFloat: bool
  var toString: bool
  while true:
    case lex.buf[lex.bufpos]
    of '0'..'9':
      add lex.token, lex.buf[lex.bufpos]
      inc lex.bufpos
    of 'a'..'z', 'A'..'Z', '_', '-':
      toString = true
      add lex.token, lex.buf[lex.bufpos]
      inc lex.bufpos
      # lex.setError("Invalid number")
      # return
    of '.':
      if toFloat: break
        # lex.setError("Invalid float number")
        # return
      toFloat = true
      add lex.token, lex.buf[lex.bufpos]
      inc lex.bufpos
    else:
      if toFloat:
        lex.kind = TK_FLOAT
      elif toString:
        lex.kind = TK_STRING
      else:
        lex.kind = TK_INTEGER
      break

proc handleString[T: Lexer](lex: var T) =
  lex.startPos = lex.getColNumber(lex.bufpos)
  lex.token = ""
  let lineno = lex.lineNumber
  inc lex.bufpos
  while true:
    case lex.buf[lex.bufpos]
    of '\\':
      lex.handleSpecial()
      if lex.hasError(): return
    of '"':
      lex.kind = TK_STRING
      inc lex.bufpos
      break
    of NewLines:
      if lex.multiLineStr:
        add lex.token, "\\\\n"
        inc lex.bufpos
      else:
        lex.setError("EOL reached before end of string")
        return
    of EndOfFile:
      lex.setError("EOF reached before end of string")
      return
    else:
      add lex.token, lex.buf[lex.bufpos]
      inc lex.bufpos
  if lex.multiLineStr:
    lex.lineNumber = lineno

proc handleCustomIdent*[T: Lexer](lex: var T, kind: TokenKind) =
  ## Handle variable declarations based the following char sets
  ## ``{'a'..'z', 'A'..'Z', '_', '-'}`` and ``{'0'..'9'}``
  lex.startPos = lex.getColNumber(lex.bufpos)
  lex.token = ""
  inc lex.bufpos
  while true:
    if lex.hasLetters(lex.bufpos):
      add lex.token, lex.buf[lex.bufpos]
      inc lex.bufpos
    elif lex.hasNumbers(lex.bufpos):
      add lex.token, lex.buf[lex.bufpos]
      inc lex.bufpos
    else:
      dec lex.bufpos
      break
  lex.setToken kind

proc handleIdent(lex: var Lexer) =
  ## Handle string-based identifiers
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
  # skip lex
  lex.generateIdentCase()           # template defined in toktok

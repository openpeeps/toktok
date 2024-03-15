# A generic tokenizer written in Nim language,
# powered by Nim's macros, zero regex and stdlib only.
#
# (c) 2023 MIT License
#     Made by Humans from OpenPeeps
#     https://github.com/openpeeps 

import std/lexbase except NewLines
import std/[macros, os, strutils, sequtils,
          tables, unicode, algorithm, streams]

export lexbase except NewLines
export streams, strutils

const
  toktokStatic {.strdefine.} = ""
  getLexerStaticPath = getProjectPath() / normalizedPath(toktokStatic)

type
  TKType = enum
    tString
    tChar
    tRange
    tVariant
    tSet
    tHandler
    tDeferred

  TKNode* = ref object
    ident: NimNode
      ## Token identifier
    case tkType: TKType
    of tString:
      stringToken: string
    of tChar:
      charToken: BiggestInt # converted to `char` later
    of tRange:
      rangeToken: tuple[x, y: NimNode]
    of tVariant:
      vFirst: TKNode
      vOthers: seq[TKNode]
    of tSet:
      sets: NimNode # type of nnkBracket
    of tHandler:
      handlerName, handlerToken: NimNode
    else: discard

  TokenModifierCallback* = proc(tkName, tkPrefix: string): string
  Settings* = object
    lexerName*, lexerTuple*, lexerTokenKind*: string
    tkModifier*: TokenModifierCallback
    tkPrefix*: string 
    keepUnknown*: bool
      ## Whether to keep unknown tokens (default false).
      ## This is useful when building a Markdown parser (for example)
    useDefaultIdent*: bool
      ## Disable default string-based handler
    useDefaultInt*: bool = true
      ## Disable default `int` handler 
    enableStaticGen: bool
      ## Generate a static `lexer.nim` file (default false)
      ## Use `-d:toktokStatic` to (re)generate lexer file.
      ## Useful in case you don't want `toktok` package
      ## to run every time you build your program.
      ##
      ## Also, due to toktok limitations, you may want to
      ## go deeper and extend the generated lexer manually.
      ##
      ## **Important!** Running `-d:toktokStatic` will overwrite
      ## your existing `lexer.nim`. Be careful
    keepChar*: bool

  Tokenizer = object
    settings: Settings
    tokens: OrderedTableRef[string, TKNode]

let
  defaultTokenModifier* {.compileTime.} = proc(tkName, tkPrefix: string): string = tkPrefix & tkName.capitalizeAscii

var
  tkUnknown {.compileTime.} = "unknown"
  tkInt {.compileTime.} = "integer"
  tkStr {.compileTime.} = "string"
  tkFloat {.compileTime.} = "float"
  tkBool {.compileTime.} = "bool"
  tkIdentDefault {.compileTime.} = "identifier"
  tkEOFStr {.compileTime.} = "eof"
  defaultPrefix {.compileTime.} = "tk_"
  customHandlers {.compileTime.} = newStmtList()

when not defined release:
  import std/[json, jsonutils]
  proc `$`(tkNode: TKNode): string {.compileTime.} =
    pretty(tkNode.toJson(), 2)

# forward declaration
# proc parseSyntax(tok: var Tokenizer, tk: NimNode) {.compileTime.}

proc getIdent(tok: var Tokenizer, id: NimNode): NimNode {.compileTime.} =
  let identStr = if id.kind == nnkAccQuoted: id[0].strVal else: id.strVal
  if tok.settings.tkModifier != nil:
    return ident(tok.settings.tkModifier(identStr, tok.settings.tkPrefix))
  ident(tok.settings.tkPrefix & identStr)

proc newTKNode(tok: var Tokenizer, tkIdent: NimNode, tkValue: NimNode): TKNode {.compileTime.} =
  case tkValue.kind:
  of nnkCharLit:
    # Register char-based tokens
    result = TKNode(tkType: tChar, ident: tkIdent, charToken: tkValue.intVal)
  of nnkStrLit:
    # Register string-based token identifiers
    result = TKNode(tkType: tString, ident: tkIdent, stringToken: tkValue.strVal)
  of nnkBracket:
    for tkv in tkValue:
      if tkv.kind != nnkStrLit: error("Invalid set", tkV)
    result = TKNode(tkType: tSet, ident: tkIdent, sets: tkValue)
  of nnkCall:
    # Register custom handlers when calling `tokenize(myHandler, '/')`
    # Otherwise, register char/string based variants.
    case tkValue[0].kind
    of nnkCharLit:
      expectKind(tkValue[1], nnkStmtList)
      var vFirst = TKNode(tkType: tChar, ident: tkIdent, charToken: tkValue[0].intVal)
      result = TKNode(tkType: tVariant, ident: tkIdent, vFirst: vFirst)
      for vToken in tkValue[1]:
        add result.vOthers, tok.newTKNode(vToken[0], vToken[1])
    of nnkStrLit:
      expectKind(tkValue[1], nnkStmtList)
      var vFirst = TKNode(tkType: tString, ident: tkIdent, stringToken: tkValue[0].strVal)
      result = TKNode(tkType: tVariant, ident: tkIdent)
      for vToken in tkValue[1]:
        add result.vOthers, (tok.newTKNode(vToken[0], vToken[1]))
    of nnkIdent:
      if tkValue.len != 3:
        error("Invalid custom handler")
      if tkValue[0].eqIdent("tokenize"):
        result = TKNode(tkType: tHandler, ident: tkIdent,
                      handlerName: ident(tkValue[1].strVal), handlerToken: tkValue[2])
      else: error("Use either `tokenize(mySlashHandle, '/')`, or `tokenizeUnicode(mySlashHandle, '/')` for adding custom handlers")
    else: error("Failed to register token node: " & $tkValue[0].kind, tkValue[0])
  of nnkInfix:
    # Tokenize ranges. Anything from `X` to `Y`.
    #
    # Use `EOL` to collect all characters from buffer from X to end of line.
    # This may be useful for implementing handlers dealing with inline comments
    #
    if tkValue[0].eqIdent(".."):
      let x =
        if tkValue[1].kind in {nnkCharLit, nnkStrLit, nnkIntLit}:
          if tkValue[1].kind == nnkCharLit:
            if char(tkValue[1].intVal) in {'0'..'9'}:
              tok.settings.useDefaultInt = false
          tkValue[1]
        else:
          error("Invalid token range for X. Expect nnkCharLit or nnkStrLit", tkValue[1])
          newLit("")
      let y = 
        if tkValue[2].kind in {nnkCharLit, nnkStrLit, nnkIntLit}:
          tkValue[2]
        elif tkValue[2].kind == nnkIdent and eqIdent(tkValue[2], "EOL"):
          newEmptyNode() # not necessary, just to add a node here
        else:
          error("Invalid token range for Y. Expect nnkCharLit, nnkStrLit or EOL", tkValue[2])
          newLit("")
      result = TKNode(tkType: tRange, ident: tkIdent, rangeToken: (x, y))
    else:
      error("Failed to register a range token. Valid example: `'/' .. EOL`", tkValue[0])
  else:
    error("Failed to register token: " & $tkValue.kind, tkValue)
    discard

#
# Compile-time utils
#
include ./macroutils

proc addField*(tkEnum: var NimNode, enumType: NimNode) =
  tkEnum.add(newEmptyNode(), enumType)

proc addField*(tkEnum: var NimNode, enumIdent, enumType: NimNode) =
  tkEnum.add(
    nnkEnumFieldDef.newTree(enumIdent, enumType)
  )

proc `?`(x, y: string): string {.compileTime.} =
  result = if x.len != 0: x else: y

proc handleNextToken(tok: var Tokenizer, tkIdent, tkLit: NimNode, tkLitLen: int): NimNode {.compileTime.} =
  nnkElifBranch.newTree(
    newCall(
      ident("next"),
      ident("lex"),
      tkLit
    ),
    newStmtList(
      newCall(
        ident("setTokenGroup"),
        ident("lex"),
        tok.getIdent(tkIdent),
        newLit(0),
        newLit(tkLitLen + 1)
      )
    )
  )

proc handleXtoEOL(tok: var Tokenizer, tkIdent, tkLit: NimNode, offset: int, wrapCond = true): NimNode {.compileTime.} =
  let x =
    newStmtList(
      # newLetStmt(
        # ident("toEOL"),
      newCall(
        ident("nextToEOL"),
        ident("lex"),
        newLit(offset),
        tok.getIdent(tkIdent)
      )
    )
  if wrapCond:
    nnkElifBranch.newTree(
      newCall(ident("next"), ident("lex"), tkLit),
      x
    )
  else:
    nnkStmtList.newTree(x)

template handleVarBranch(branch: var NimNode) =
  tkNode.vOthers.reverse()
  for vOther in tkNode.vOthers:
    let id = tok.getIdent(vOther.ident)
    case vOther.tkType
    of tString:
      tkEnum.addField(id)
      add branch, tok.handleNextToken(vOther.ident, newLit(vOther.stringToken), vOther.stringToken.len)
    of tChar:
      tkEnum.addField(id)
      add branch, tok.handleNextToken(vOther.ident, newLit(char(vOther.charToken)), 1)
    of tRange:
      tkEnum.addField(id)
      if vOther.rangeToken.x.kind == nnkCharLit:
        if vOther.rangeToken.y.kind == nnkEmpty:
          # Handle Ranges, from X char to end of line (X .. EOL)
          branch.add(tok.handleXtoEOL(vOther.ident, vOther.rangeToken.x, 2))  
      elif vOther.rangeToken.x.kind == nnkStrLit:
        if vOther.rangeToken.y.kind == nnkEmpty:
          # Handle Ranges, from X string to end of line (X .. EOL)
          branch.add(tok.handleXtoEOL(vOther.ident, vOther.rangeToken.x, len(vOther.rangeToken.x.strVal)))
      else: discard # todo make it work from X .. Y
    of tHandler:
      tkEnum.addField(id)
      if vOther.handlerToken.kind in {nnkCharLit, nnkStrLit}:
        # add custom handler to the main case statement
        branch.add(
          nnkElifBranch.newTree(
            newCall(
              ident("next"),
              ident("lex"),
              vOther.handlerToken
            ),
            newStmtList(
              newCall(vOther.handlerName, ident("lex"), id)
            )
          )
        )
      else: discard
    else: 
      echo "todo"
  let callElseBranch = newCall(ident("setToken"), ident("lex"), tok.getIdent(tkNode.vFirst.ident))
  branch.add(nnkElse.newTree(newStmtList(callElseBranch)))

macro handlers*(custom: untyped) =
  ## Define your own handlers. For example:
  ##```nim
  ##  proc handleClass(lex: var Lexer, kind: `TokenKind`) =
  ##    # your code
  ##```
  expectKind(custom, nnkStmtList)
  customHandlers = custom

const defaultSettings* =
  Settings(
    tkPrefix: "tk",
    lexerName: "Lexer",
    lexerTuple: "TokenTuple",
    lexerTokenKind: "TokenKind",
    tkModifier: defaultTokenModifier,      
    useDefaultIdent: true,
    enableStaticGen: toktokStatic.len > 0,
    keepUnknown: true,
    keepChar: false,
  )

macro registerTokens*(settings: static Settings, tokens: untyped) =
  tokens.expectKind(nnkStmtList)
  var
    tok = Tokenizer(tokens: newOrderedTable[string, TKNode](), settings: settings)
    tkEnum = newNimNode(nnkEnumTy)
    mainBranches, identBranches: seq[tuple[cond, body: NimNode]]
    getDefaultTokenCondBody = nnkIfStmt.newTree()
  let tokenKindEnumName = tok.settings.lexerTokenKind
  for tk in tokens:
    case tk.kind
    of nnkAsgn:
      # handle known tokens (assigned value) 
      case tk[0].kind:
      of nnkIdent:
        tok.tokens[tk[0].strVal] = tok.newTKNode(tk[0], tk[1])
      of nnkAccQuoted:
        tok.tokens[tk[0][0].strVal] = tok.newTKNode(tk[0][0], tk[1])
      else: error("Invalid token. Expect `nnkIdent` or `nnkAccQuoted`. Got `" & $tk.kind & "`", tk[0])
    of nnkIdent:
      # handle deferred tokens
      tok.tokens[tk.strVal] = TKNode(tkType: tDeferred, ident: tk)
    of nnkAccQuoted:
      tok.tokens[tk[0].strVal] = TKNode(tkType: tDeferred, ident: tk[0])
    else: discard
  # define default fields for `TokenKind` enum
  for default in [(tkUnknown, "unknown"), (tkInt, "integer"), (tkFloat, "float"),
                (tkStr, "string"), (tkIdentDefault, "identifier"), (tkEOFStr, "EOF")]:
    tkEnum.addField(tok.getIdent(newLit default[0]))
    getDefaultTokenCondBody.add(
      nnkElifBranch.newTree(
        nnkInfix.newTree(
          ident("=="),
          ident("s"),
          newLit(default[1])
        ),
        newStmtList(
          newAssignment(
            ident("result"),
            tok.getIdent(newLit default[0])
          )
        )
      )
    )
  tok.tokens.sort(system.cmp) # sort enum by name A-Z
  for name, tkNode in tok.tokens:
    # Generate a ``TokenKind`` enumeration we store all tokens.
    #
    # Optionally, you can switch to hash tables,
    # which allows you to create tokenizers at runtime
    var tkIdent = tok.getIdent(tkNode.ident)
    case tkNode.tkType
    of tString:
      tkEnum.addField(tkIdent)
      identBranches.add((
        newLit(tkNode.stringToken),
        newStmtList(tkIdent)
      ))
    of tChar:
      tkEnum.addField(tkIdent)
      var mainCharBody = newNimNode(nnkStmtList)
      if tok.settings.keepChar:
        mainCharBody.add(
          nnkCommand.newTree(
            ident("add"),
            newDotExpr(ident("lex"), ident("token")),
            nnkBracketExpr.newTree(
              newDotExpr(ident("lex"), ident("buf")),
              newDotExpr(ident("lex"), ident("bufpos")),
            )
          )
        )
      mainCharBody.add(
        newCall(
          newDotExpr(ident("lex"), ident("setToken")),
          tkIdent,
          newLit(1)
        ),
      )
      mainBranches.add((
        cond: newLit(char(tkNode.charToken)),
        body: mainCharBody
      ))
    of tSet:
      tkEnum.addField(tkIdent)
      identBranches.add((
        cond: tkNode.sets,
        body: tkIdent
      ))
    of tVariant:
      tkEnum.addField(tkIdent)
      var variantBranches = newNimNode(nnkIfStmt)
      case tkNode.vFirst.tkType
      of tString:
        # Handle string-based variants:
        # of "public":
        #   if next(lex, "class"):
        #     setTokenGroup(lex, tkPubClass, 11, 11)
        #   elif next(lex, "function"):
        #     setTokenGroup(lex, tkPubFn, 14, 14)
        variantBranches.handleVarBranch()
        identBranches.add((
          cond: newLit(tkNode.vFirst.stringToken),
          body: variantBranches
        ))
      of tChar:
        # Handle char-based variants, for example:
        # of '=':
        #   if next(lex, "="):
        #     setTokenGroup(lex, tkEQ, 2, 2)
        #   else:
        #     setToken(lex, tkAssgn)
        variantBranches.handleVarBranch()
        mainBranches.add((
          cond: newLit(char(tkNode.vFirst.charToken)),
          body: variantBranches
        ))
      else: discard
    of tRange:
      tkEnum.addField(tkIdent)
      if tkNode.rangeToken.x.kind == nnkCharLit:
        var cond = newNimNode(nnkIfStmt)
        mainBranches.add((
          cond: tkNode.rangeToken.x,
          body: tok.handleXtoEOL(tkNode.ident, tkNode.rangeToken.x, 0, false)
        ))
    of tHandler:
      tkEnum.addField(tkIdent)
      if tkNode.handlerToken.kind == nnkCharLit:
        # add custom handler to the main case statement
        mainBranches.add((
          cond: tkNode.handlerToken,
          body: newCall(tkNode.handlerName, ident("lex"), tkIdent)
        ))
      elif tkNode.handlerToken.kind == nnkInfix:
        mainBranches.add((
          cond: tkNode.handlerToken,
          body: newCall(tkNode.handlerName, ident("lex"), tkIdent)
        )) 
      else:
        identBranches.add((
          cond: tkNode.handlerToken,
          body: newStmtList(
            newCall(tkNode.handlerName, ident("lex"), tkIdent),
            newDotExpr(ident("lex"), ident("kind"))
          )
        ))
    of tDeferred:
      tkEnum.addField(tkIdent)

  var typeSection = newNimNode(nnkTypeSection)
  add typeSection,
    nnkTypeDef.newTree(
      nnkPragmaExpr.newTree(
        nnkPostFix.newTree(
          ident("*"),
          ident(tok.settings.lexerTokenKind)
        ),
        newTree(nnkPragma).add(ident("pure")) # not sure if is necessary
      ),
      newEmptyNode(),
      tkEnum
    )
  add typeSection,
    newTupleType(
      tok.settings.lexerTuple, public = true,
      fields = [
        ("kind", ident(tok.settings.lexerTokenKind)),
        ("value", ident("string")),
        ("wsno|line|col|pos", ident("int")),
        ("attr", nnkBracketExpr.newTree(ident("seq"), ident("string")))
      ]
    )

  # Create `Lexer` object from `BaseLexer` with fields
  add typeSection,
    newObject(id = tok.settings.lexerName, parent = "BaseLexer", public = true,
      fields = [
        ("kind", ident(tok.settings.lexerTokenKind), false),
        ("token", ident("string"), false),
        ("attr", nnkBracketExpr.newTree(ident("seq"), ident("string")), false),
        ("error", ident("string"), false),
        ("startPos", ident("int"), false),
        ("wsno", ident("int"), false),
        ("multiLineStr", ident("bool"), false)
      ]
    )
  # Create `LexerException`
  typeSection.add(newObject(tok.settings.lexerName & "Exception", parent = "CatchableError", public = true))
  let caseOfChar = nnkBracketExpr.newTree(newDotExpr(ident("lex"), ident("buf")), newDotExpr(ident("lex"), ident("bufpos")))
  # Handle {'a'..'z', 'A'..'Z', '_'}
  mainBranches.add((
    cond: nnkCurly.newTree(
      newTree(nnkInfix, ident(".."), newLit('a'), newLit('z')),
      newTree(nnkInfix, ident(".."), newLit('A'), newLit('Z')),
      newLit('_')
    ),
    body: newStmtList(newCall(ident("handleIdent"), ident("lex")))
  ))
  # Handle {'0'..'9'}
  if tok.settings.useDefaultInt:
    mainBranches.add((
      cond: nnkCurly.newTree(newTree(nnkInfix, ident(".."), newLit('0'), newLit('9'))),
      body: newStmtList(newCall(ident("handleNumber"), ident("lex")))
    ))
  # Handle "double quote strings"
  mainBranches.add((
    cond: newLit('\"'),
    body: newStmtList(newCall(ident("handleString"), ident("lex")))
  ))
  # Handle EOF
  mainBranches.add((
    cond: ident "EndOfFile",
    body: newStmtList(
      newAssignment(
        newDotExpr(ident("lex"), ident("startPos")),
        newCall(
          newDotExpr(ident("lex"), ident("getColNumber")),
          newDotExpr(ident("lex"), ident("bufpos")),
        )
      ),
      newAssignment(
        newDotExpr(ident "lex", ident "kind"),
        tok.getIdent(newLit "eof")
      )
    )
  ))
  let caseOfElse =
    if tok.settings.keepUnknown:
      newStmtList(
        newAssignment(
          newDotExpr(ident "lex", ident "token"),
          nnkPrefix.newTree(
            ident "$",
            nnkPar.newTree(
              nnkBracketExpr.newTree(
                newDotExpr(ident "lex", ident "buf"),
                newDotExpr(ident "lex", ident "bufpos")
              )
            )
          )
        ),
        newCall(
          ident("setToken"),
          ident("lex"),
          getIdent(tok, newLit(tkUnknown)),
          newLit(1)
        ),
      )
    else:
      newStmtList(
        newCall(
          newDotExpr(ident("lex"), ident("setToken")),
          newDotExpr(ident(tok.settings.lexerTokenKind), getIdent(tok, newLit(tkUnknown)))
        )
      )

  result = newStmtList()
  result.add typeSection
  result.add newProc(
    id = "getDefaultToken",
    params = [("s", "string", false)],
    public = false,
    body = getDefaultTokenCondBody,
    returnType = ident(tok.settings.lexerTokenKind)
  )

  let LexerName = ident(tok.settings.lexerName)
  let TokenKind = ident(tok.settings.lexerTokenKind)
  result.add quote do:
    const azAZ = Letters + {'_', '-'}
    proc newLexer*(fileContent: string, allowMultilineStrings = false): `LexerName` =
      ## Initialize a new BaseLexer instance with given Stream
      var lex = `LexerName`()
      open(lex, newStringStream(fileContent))
      lex.startPos = 0
      lex.kind = getDefaultToken("unknown")
      lex.token = ""
      lex.error = ""
      lex.multiLineStr = allowMultilineStrings
      result = lex

    proc lexReady*(lex: var `LexerName`) =
      lex.startPos = lex.getColNumber(lex.bufpos)
      setLen(lex.token, 0); setLen(lex.attr, 0)

    proc inc*(lex: var `LexerName`, offset = 1) = inc lex.bufpos, offset
    proc current*(lex: var `LexerName`): char = result = lex.buf[lex.bufpos]
    
    proc add*(lex: var `LexerName`) =
      add lex.token, lex.buf[lex.bufpos]
      inc lex
    
    proc setError*(lex: var `LexerName`; message: string) =
      lex.kind = getDefaultToken("unknown")
      if lex.error.len == 0:
        lex.error = message
    
    proc hasError*(lex: `LexerName`): bool = lex.error.len != 0
    proc getError*(lex: `LexerName`): string = "($1:$2) $3" % [$lex.lineNumber, $(lex.startPos), lex.error]
  
    proc handleNewLine(lex: var `LexerName`) =
      ## Handle new lines
      case lex.buf[lex.bufpos]
      of '\c': lex.bufpos = lex.handleCR(lex.bufpos)
      of '\n': lex.bufpos = lex.handleLF(lex.bufpos)
      else: discard

    proc inBuffer(lex: var `LexerName`, pos: int, chars: set[char]): bool =
      try:
        result = lex.buf[pos] in chars
      except IndexDefect:
        result = false

    proc hasLetters*(lex: var `LexerName`, pos: int): bool =
      lex.inBuffer(pos, azAZ)

    proc hasNumbers*(lex: var `LexerName`, pos: int): bool =
      lex.inBuffer(pos, Digits)

    proc skip*(lex: var `LexerName`) =
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

    proc setToken(lex: var `LexerName`, tokenKind: `TokenKind`, offset = 1, initPos = - 1) =
      ## Set meta data for current token
      lex.kind = tokenKind
      lex.startPos =
        if initPos == -1:
          lex.getColNumber(lex.bufpos)
        else:
          initPos
      inc(lex.bufpos, offset)

    proc setTokenGroup(lex: var `LexerName`, tokenKind: `TokenKind`, offset = 0, multichars = 0) =
      ## Set token with multiple characters
      lex.startPos = lex.getColNumber(lex.bufpos)
      var i = 0
      if multichars != 0:
        while i < multichars:
          add lex.token, lex.buf[lex.bufpos]
          inc lex.bufpos
          inc i
      else:
        add lex.token, lex.buf[lex.bufpos]
        inc lex.bufpos, offset
      lex.kind = tokenKind

    proc nextToEOL(lex: var `LexerName`, offset = 1, tokenKind: `TokenKind`) =
      # Collect from pos ition X to EOL.
      # Mainly used to tokenize single line comments
      lexReady lex
      inc lex.bufpos, offset
      while true:
        case lex.buf[lex.bufpos]:
        of NewLines, EndOfFile:
          lex.handleNewLine()
          break
        else: 
          add lex
      lex.kind = tokenKind
      lex.token = strutils.strip(lex.token)

    proc handleSpecial(lex: var `LexerName`) =
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
        add lex.token, "\""
        inc lex.bufpos
      else:
        lex.setError("Unknown escape sequence: '\\" & lex.buf[lex.bufpos] & "'")

    proc next(lex: var `LexerName`, ch: char, offset = 1): bool =
      ## Check next char without modifying bufpos
      # skip lex # loose wsno info
      try: result = lex.buf[lex.bufpos + offset] in {ch}
      except IndexDefect: discard

    proc next(lex: var `LexerName`, chars: string): bool =
      # Check next characters without modifying bufpos
      var i = 1
      var status = false
      for c in chars:
        status = lex.next(c, i)
        if status == false:
          return status
        inc i
      result = status

    proc nextToSpec(lex: var `LexerName`, endChar: char, tokenKind: `TokenKind`, str = "") =
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

    proc nextToSpec(lex: var `LexerName`, endChar: string, tokenKind: `TokenKind`) =
      lex.nextToSpec(endChar[^1], tokenKind, endChar)

    proc handleNumber(lex: var `LexerName`) =
      # Handle integers and float numbers
      setLen(lex.token, 0)
      lex.startPos = lex.getColNumber(lex.bufpos)
      var toString, toFloat: bool
      while true:
        case lex.buf[lex.bufpos]
        of '0'..'9':
          add lex.token, lex.buf[lex.bufpos]
          inc lex.bufpos
        of 'a'..'z', 'A'..'Z', '_', '-':
          toString = true
          add lex.token, lex.buf[lex.bufpos]
          inc lex.bufpos
        of '.':
          if toFloat: break
          try:
            if lex.buf[lex.bufpos + 1] in {'0'..'9'}:
              toFloat = true
            else:
              lex.kind = getDefaultToken("integer")
              break
          except IndexDefect:
            toString = true
          add lex.token, lex.buf[lex.bufpos]
          inc lex.bufpos
        else:
          if toFloat:
            lex.kind = getDefaultToken("float")
          elif toString:
            lex.kind = getDefaultToken("string")
          else:
            lex.kind = getDefaultToken("integer")
          break

    proc handleString(lex: var `LexerName`) =
      # Handle strings
      lex.startPos = lex.getColNumber(lex.bufpos)
      setLen(lex.token, 0)
      let lineno = lex.lineNumber
      let bufpos = lex.bufpos
      var blockString: bool
      if lex.next("\"\""):
        inc lex.bufpos, 3
        blockString = true
      else:
        inc lex.bufpos
      while true:
        case lex.buf[lex.bufpos]
        of '\\':
          lex.handleSpecial()
          if lex.hasError(): return
        of '"':
          if lex.next("\"\"") and blockString:
            lex.kind = getDefaultToken("string")
            inc lex.bufpos, 3
            break
          elif blockString:
            while true:
              case lex.buf[lex.bufpos]
              of '"':
                if lex.next("\"\""):
                  lex.kind = getDefaultToken("string")
                  inc lex.bufpos, 3
                  break
                else:
                  add lex.token, lex.buf[lex.bufpos]
                  inc lex.bufpos
              of NewLines:
                  inc lex.lineNumber
                  inc lex.bufpos
              else:
                add lex.token, lex.buf[lex.bufpos]
                inc lex.bufpos
            break
          else:
            lex.kind = getDefaultToken("string")
            inc lex.bufpos
            break
        of NewLines:
          if lex.multiLineStr:
            inc lex.lineNumber
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

  if customHandlers.len > 0:
    result.add customHandlers

  # Create `handleIdentCase` compile-time procedure
  let caseOfIdent = newDotExpr(ident "lex", ident "token")
  let identElseBranch =
    if tok.settings.useDefaultIdent:
      newStmtList(tok.getIdent(newLit tkIdentDefault))
    else:
      newStmtList(newCall(newDotExpr(ident("lex"), ident("handleCustomIdent"))))
  result.add newProc(
    id = "handleIdentCase",
    params = [("lex", tok.settings.lexerName, true)],
    body = newAssignment(
      newDotExpr(ident("lex"), ident("kind")),
      newCaseStmt(caseOfIdent, identBranches, identElseBranch)
    )
  )
  result.add quote do:
    proc handleIdent(lex: var `LexerName`) =
      ## Handle string-based identifiers
      lexReady lex
      while true:
        if lex.hasLetters(lex.bufpos):
          add lex.token, lex.buf[lex.bufpos]
          inc lex.bufpos
        elif lex.hasNumbers(lex.bufpos):
          add lex.token, lex.buf[lex.bufpos]
          inc lex.bufpos
        else: break
      # skip lex
      lex.handleIdentCase() # generated with macros

  # Create `handleMainCase` compile-time procedure
  result.add newProc(
    id = "handleMainCase",
    params = [("lex", tok.settings.lexerName, true)],
    body = newCaseStmt(caseOfChar, mainBranches, caseOfElse)
  )
  # Create `getToken` runtime procedure
  var tupleConstr = nnkTupleConstr.newTree()
  for tp in [("kind", "kind"), ("value", "token"), ("wsno", "wsno"),
            ("line", "lineNumber"), ("col", "startPos"),
            ("pos", "startPos"), ("attr", "attr")]:
    var dotExpr = newDotExpr(ident("lex"), ident(tp[1]))
    if tp[0] == "col":
      dotExpr = nnkInfix.newTree(ident("+"), dotExpr, newLit(1)) # + 1 to col field to reflect IDE
    add tupleConstr, nnkExprColonExpr.newTree(ident(tp[0]), dotExpr)
  var getTokenStmt =
    nnkStmtList.newTree(
      newAssignment(
        newDotExpr(ident("lex"), ident("startPos")),
        newCall(ident("getColNumber"), ident("lex"), newDotExpr(ident("lex"), ident("bufpos")))
      ),
      newCall(
        ident("setLen"),
        newDotExpr(ident("lex"), ident("token")),
        newLit(0)
      ),
      newCall(ident("skip"), ident("lex"))
    )
  getTokenStmt.add(newCall(ident("handleMainCase"), ident("lex")))
  if not tok.settings.keepUnknown:
    getTokenStmt.add(
      newLetStmt(
        ident("unknownToken"),
        nnkBracketExpr.newTree(
          newDotExpr(ident("lex"), ident("buf")),
          newDotExpr(ident("lex"), ident("bufpos")),
        )
      )
    )
    getTokenStmt.add(
      newIfStmt(
        (
          nnkInfix.newTree(
            ident("=="),
            newDotExpr(ident("lex"), ident("kind")),
            getIdent(tok, newLit(tkUnknown))
          ),
          newCall(
            ident("setError"),
            ident("lex"),
            nnkInfix.newTree(
              ident("%"), newLit("Unexpected token $1"),
              nnkBracket.newTree(
                nnkPrefix.newTree(ident("$"), ident("unknownToken"))
              )
            )
          )
        )
    ))
  getTokenStmt.add(nnkReturnStmt.newTree(tupleConstr))
  result.add(newProc(
    id = "getToken",
    public = true,
    returnType = ident(tok.settings.lexerTuple),
    params = [
      ("lex", tok.settings.lexerName, true)
    ],
    getTokenStmt
  ))

  when defined toktokdebug:
    echo result.repr
  when defined toktokStatic:
    if toktokStatic.endsWith(".nim"):
      # todo find a better method
      var output =  """
import std/lexbase except NewLines
import std/[strutils, streams]
"""
      add output, replace(result.repr, "`gensym0", "").replace("`gensym1", "")
      writeFile(getLexerStaticPath, output)
      result = newStmtList()
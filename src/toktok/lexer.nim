# A generic tokenizer written in Nim language,
# powered by Nim's macros, zero regex and stdlib only.
#
# (c) 2023 MIT License
#     Made by Humans from OpenPeeps
#     https://github.com/openpeeps 

import std/lexbase except NewLines
import std/[macros, strutils, sequtils,
          tables, unicode, algorithm, streams]

export lexbase, streams

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
    tkModifier*: TokenModifierCallback
    tkPrefix*: string 
    enableKeepUnknown*, enableCustomIdent*: bool

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
  tKindEnumName {.compileTime.} = ident("TokenKind")
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
      newLetStmt(
        ident("toEOL"),
        newCall(ident("nextToEOL"), ident("lex"), newLit(offset + 1))
      ),
      newCall(
        ident("setToken"), ident("lex"), tok.getIdent(tkIdent),
        newDotExpr(ident("toEol"), ident("pos")),
        newDotExpr(ident("toEol"), ident("initPos"))
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
    case vOther.tkType
    of tString:
      tkEnum.addField(tok.getIdent(vOther.ident))
      add branch, tok.handleNextToken(vOther.ident, newLit(vOther.stringToken), vOther.stringToken.len)
    of tChar:
      tkEnum.addField(tok.getIdent(vOther.ident))
      add branch, tok.handleNextToken(vOther.ident, newLit(char(vOther.charToken)), 1)
    of tRange:
      tkEnum.addField(tok.getIdent(vOther.ident))
      if vOther.rangeToken.x.kind == nnkCharLit:
        if vOther.rangeToken.y.kind == nnkEmpty:
          # Handle Ranges, from X char to end of line (X .. EOL)
          branch.add(tok.handleXtoEOL(vOther.ident, vOther.rangeToken.x, 1))  
      elif vOther.rangeToken.x.kind == nnkStrLit:
        if vOther.rangeToken.y.kind == nnkEmpty:
          # Handle Ranges, from X string to end of line (X .. EOL)
          branch.add(tok.handleXtoEOL(vOther.ident, vOther.rangeToken.x, len(vOther.rangeToken.x.strVal)))
    else: discard
  let callElseBranch = newCall(ident("setToken"), ident("lex"), tok.getIdent(tkNode.vFirst.ident))
  branch.add(nnkElse.newTree(newStmtList(callElseBranch)))

macro handlers*(custom: untyped) =
  ##Define your own handlers. For example:
  ##```nim
  ##  proc handleClass(lex: var Lexer, kind: TokenKind) =
  ##    # your code
  ##```
  expectKind(custom, nnkStmtList)
  customHandlers = custom

const defaultSettings* =
  Settings(
    tkPrefix: "tk",
    tkModifier: defaultTokenModifier,      
    enableKeepUnknown: true,
    enableCustomIdent: false
  )

macro registerTokens*(settings: static Settings, tokens: untyped) =
  tokens.expectKind(nnkStmtList)
  var
    tok = Tokenizer(tokens: newOrderedTable[string, TKNode](), settings: settings)
    tkEnum = newNimNode(nnkEnumTy)
    mainBranches, identBranches: seq[tuple[cond, body: NimNode]]
    getDefaultTokenCondBody = nnkIfStmt.newTree()
  let tokenKindEnumName = tKindEnumName.strVal
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
  # define default fields for TokenKind enum
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
    # Generate a `TokenKind` enumeration we store all tokens.
    #
    # Optionally, you can switch to hash tables,
    # which allows you to create dynamic tokenizers
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
      mainBranches.add((
        cond: newLit(char(tkNode.charToken)),
        body: newCall(
          newDotExpr(ident("lex"), ident("setToken")),
          tkIdent,
          newLit(1)
        )
      ))
    of tSet:
      tkEnum.addField(tkIdent)
      identBranches.add((
        cond: tkNode.sets,
        body: tkIdent
      ))
    of tVariant:
      tkEnum.addField(tkIdent)
      case tkNode.vFirst.tkType
      of tString:
        var variantBranches = newNimNode(nnkIfStmt)
        variantBranches.handleVarBranch()
        identBranches.add((
          cond: newLit(tkNode.vFirst.stringToken),
          body: variantBranches
        ))
      of tChar:
        # Handle char-based variants, for example:
        # of '=':
        #   if next(lex, "="):
        #     setTokenGroup(lex, TK_EQ, 2, 2)
        #   else:
        #     setToken(lex, TK_ASSIGN)
        var variantBranches = newNimNode(nnkIfStmt)
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
          cond: newLit(char(tkNode.rangeToken.x.intVal)),
          body: tok.handleXtoEOL(tkNode.ident, tkNode.rangeToken.x, 0, false)
        ))
    of tHandler:
      tkEnum.addField(tkIdent)
      if tkNode.handlerToken.kind == nnkCharLit:
        # add custom handler to the main case statement
        mainBranches.add((
          cond: newLit(char(tkNode.handlerToken.intVal)),
          body: newCall(tkNode.handlerName, ident("lex"), tkIdent)
        ))
      else:
        # a string based custom handler
        discard
    of tDeferred:
      tkEnum.addField(tkIdent)

  var typeSection = newNimNode(nnkTypeSection)
  add typeSection,
    nnkTypeDef.newTree(
      nnkPragmaExpr.newTree(
        nnkPostFix.newTree(
          ident("*"),
          tKindEnumName
        ),
        newTree(nnkPragma).add(ident("pure")) # not sure if is necessary
      ),
      newEmptyNode(),
      tkEnum
    )
  add typeSection,
    newTupleType(
      "TokenTuple", public = true,
      fields = [
        ("kind", ident(tokenKindEnumName)),
        ("value", ident("string")),
        ("wsno|line|col|pos", ident("int")),
        ("attr", nnkBracketExpr.newTree(ident("seq"), ident("string")))
      ]
    )

  # Create `Lexer` object from `BaseLexer` with fields
  add typeSection,
    newObject(id = "Lexer", parent = "BaseLexer", public = true,
      fields = [
        ("kind", ident(tokenKindEnumName)),
        ("token", ident("string")),
        ("attr", nnkBracketExpr.newTree(ident("seq"), ident("string"))),
        ("error", ident("string")),
        ("startPos", ident("int")),
        ("wsno", ident("int")),
        ("multiLineStr", ident("bool"))
      ]
    )
  # Create `LexerException`
  typeSection.add(newObject("LexerException", parent = "CatchableError", public = true))
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
    if tok.settings.enableKeepUnknown:
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
          newDotExpr(ident(tokenKindEnumName), getIdent(tok, newLit(tkUnknown)))
        )
      )

  result = newStmtList()
  result.add typeSection
  result.add newProc(
    id = "getDefaultToken",
    params = [("s", "string", false)],
    public = false,
    body = getDefaultTokenCondBody,
    returnType = ident(tokenKindEnumName)
  )
  
  # Include ./lexutils
  result.add newInclude("./lexutils")
  
  if customHandlers.len > 0:
    result.add customHandlers
  
  # Create `handleIdentCase` compile-time procedure
  let caseOfIdent = newDotExpr(ident "lex", ident "token")
  let identElseBranch =
    if tok.settings.enableCustomIdent:
      newStmtList(newCall(newDotExpr(ident("lex"), ident("handleCustomIdent"))))
    else:
      newStmtList(tok.getIdent(newLit tkIdentDefault))
  result.add newProc(
    id = "handleIdentCase",
    params = [("lex", "Lexer", true)],
    body = newAssignment(
      newDotExpr(ident("lex"), ident("kind")),
      newCaseStmt(caseOfIdent, identBranches, identElseBranch)
    )
  )
  # Create `handleMainCase` compile-time procedure
  result.add newProc(
    id = "handleMainCase",
    params = [("lex", "Lexer", true)],
    body = newCaseStmt(caseOfChar, mainBranches, caseOfElse)
  )
  # Create `getToken` runtime procedure
  var tupleConstr = nnkTupleConstr.newTree()
  for tp in [("kind", "kind"), ("value", "token"), ("wsno", "wsno"),
            ("line", "lineNumber"), ("col", "startPos"), ("pos", "startPos"), ("attr", "attr")]:
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
  if not tok.settings.enableKeepUnknown:
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
    returnType = ident("TokenTuple"),
    params = [
      ("lex", "Lexer", true)
    ],
    getTokenStmt
  ))
  when defined toktokdebug:
    echo result.repr
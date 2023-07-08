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
    tEol
    tVariant

  TKNode* = ref object
    ident: NimNode
      ## Token identifier
    case tkType: TKType
    of tString:
      strTk: string
    of tChar:
      charTk: BiggestInt # converted to `char` later
    of tRange:
      vrange: tuple[x, y: NimNode]
    of tEol:
      lineTk: TKNode
    of tVariant:
      vFirst: TKNode
      vOthers: seq[TKNode]

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
    result = TKNode(tkType: tChar, ident: tkIdent, charTk: tkValue.intVal)
  of nnkStrLit:
    # Register string-based token identifiers
    result = TKNode(tkType: tString, ident: tkIdent, strTk: tkValue.strVal)
  of nnkCall:
    case tkValue[0].kind
    of nnkCharLit:
      expectKind(tkValue[1], nnkStmtList)
      var vFirst = TKNode(tkType: tChar, ident: tkIdent, charTk: tkValue[0].intVal)
      result = TKNode(tkType: tVariant, ident: tkIdent, vFirst: vFirst)
      for vToken in tkValue[1]:
        add result.vOthers, tok.newTKNode(vToken[0], vToken[1])
    of nnkStrLit:
      expectKind(tkValue[1], nnkStmtList)
      var vFirst = TKNode(tkType: tString, ident: tkIdent, strTk: tkValue[0].strVal)
      result = TKNode(tkType: tVariant, ident: tkIdent)
      # echo result.ident
      for vToken in tkValue[1]:
        add result.vOthers, (tok.newTKNode(vToken[0], vToken[1]))
    of nnkIdent:
      # Handle variant-based tokens, something similar with
      # ...
      # of '/':
      #   if next(lex, '/'): tkInlineComment
      #   else: tkDivide
      echo tkValue.treeRepr()
      echo tkValue.kind
      # Register custom based handlers
    else:
      error("Failed to register token node: " & $tkValue[0].kind, tkValue[0])
  of nnkInfix:
    if tkValue[0].eqIdent(".."):
      let x =
        if tkValue[1].kind == nnkCharLit:   newLit($(char(tkValue[1].intVal)))
        elif tkValue[1].kind == nnkStrLit:  tkValue[1]
        else:
          error("Invalid token range for X. Expect nnkCharLit or nnkStrLit", tkValue[1])
          newLit("")
      let y = 
        if tkValue[2].kind == nnkCharLit:   newLit($(char(tkValue[2].intVal)))
        elif tkValue[2].kind == nnkStrLit:  tkValue[2]
        elif tkValue[2].kind == nnkIdent and eqIdent(tkValue[2], "EOL"):
          newEmptyNode() # not necessary, just to add a node here
        else:
          error("Invalid token range for Y. Expect nnkCharLit, nnkStrLit or EOL", tkValue[2])
          newLit("")
      result = TKNode(tkType: tRange, ident: tkIdent, vrange: (x, y))
    else:
      error("Failed to register a range token. Valid example: `'/' .. EOL`", tkValue[0])
  else:
    error("Failed to register token: " & $tkValue.kind, tkValue)
    discard

#
# Compile-time utils
#
include ./macroutils

#
# A public compile-time macro
# to register tokens in a fancy way
#
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

proc handleNextTokenEndOfLine(tok: var Tokenizer, tkIdent, tkLit: NimNode, offset: int): NimNode {.compileTime.} =
  nnkElifBranch.newTree(
    newCall(ident("next"), ident("lex"), tkLit),
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
  )

template handleVarBranch(branch: var NimNode) =
  tkNode.vOthers.reverse()
  for vOther in tkNode.vOthers:
    case vOther.tkType
    of tString:
      tkEnum.addField(tok.getIdent(vOther.ident), newLit(vOther.strTK))
      add branch, tok.handleNextToken(vOther.ident, newLit(vOther.strTK), vOther.strTK.len)
    of tChar:
      tkEnum.addField(tok.getIdent(vOther.ident))
      add branch, tok.handleNextToken(vOther.ident, newLit(char(vOther.charTK)), 1)
    of tRange:
      tkEnum.addField(tok.getIdent(vOther.ident))
      if vOther.vrange.x.kind == nnkStrLit:
        if vOther.vrange.y.kind == nnkEmpty:
          # Handle Ranges, from X string to end of line (X .. EOL)
          add branch,
            tok.handleNextTokenEndOfLine(vOther.ident, vOther.vrange.x, len(vOther.vrange.x.strVal))
        discard
    else: discard
  branch.add(
    nnkElse.newTree(
      newStmtList(
        newCall(
          ident("setToken"),
          ident("lex"),
          tok.getIdent(tkNode.vFirst.ident)
        )
      )
    )
  )

macro handlers*(cHandlers) =
  customHandlers = cHandlers

macro registerTokens*(settings: static Settings, tokens: untyped) =
  var tok = Tokenizer(tokens: newOrderedTable[string, TKNode](), settings: settings)
  let tokenKindEnumName = tKindEnumName.strVal
  
  tokens.expectKind(nnkStmtList)
  for tk in tokens:
    case tk.kind
    of nnkAsgn:
      var node: TKNode
      case tk[0].kind:
      of nnkIdent:
        tok.tokens[tk[0].strVal] = tok.newTKNode(tk[0], tk[1])
      of nnkAccQuoted:
        tok.tokens[tk[0][0].strVal] = tok.newTKNode(tk[0][0], tk[1])
      else: error("Invalid token. Expect `nnkIdent` or `nnkAccQuoted`. Got `" & $tk.kind & "`", tk[0])
    else:
      discard

  var
    tkEnum = newNimNode(nnkEnumTy)
    mainBranches, identBranches: seq[tuple[cond, body: NimNode]]
    getDefaultTokenCondBody = nnkIfStmt.newTree()

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
    var tkIdent = tok.getIdent(newLit name)
    case tkNode.tkType
    of tString:
      tkEnum.addField(tkIdent, newLit(tkNode.strTk))
      identBranches.add((
        newLit(tkNode.strTk),
        newStmtList(tok.getIdent(tkNode.ident))
      ))
    of tChar:
      tkEnum.addField(tok.getIdent(newLit name))
      mainBranches.add((
        newLit(char(tkNode.charTk)),
        newCall(
          newDotExpr(ident("lex"), ident("setToken")),
          tkIdent,
          newLit(1)
        )
      ))
    of tVariant:
      case tkNode.vFirst.tkType
      of tString:
        tkEnum.addField(tkIdent, newLit(tkNode.vFirst.strTk))
        var variantBranches = newNimNode(nnkIfStmt)
        variantBranches.handleVarBranch()
        identBranches.add((
          newLit(tkNode.vFirst.strTk),
          variantBranches
          # newStmtList(tok.getIdent(tkNode.vFirst.ident))
        ))
      of tChar:
        # Handle char-based variants, for example:
        # of '=':
        #   if next(lex, "="):
        #     setTokenGroup(lex, TK_EQ, 2, 2)
        #   else:
        #     setToken(lex, TK_ASSIGN)
        tkEnum.addField(tkIdent)
        var variantBranches = newNimNode(nnkIfStmt)
        variantBranches.handleVarBranch()
        mainBranches.add((
          newLit(char(tkNode.vFirst.charTk)),
          variantBranches
        ))
      else: discard
    of tRange:
      echo "X"
    of tEOL:
      discard

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
        ("kind", tokenKindEnumName),
        ("value", "string"),
        ("wsno|line|col|pos", "int")
      ]
    )

  # Create `Lexer` object from `BaseLexer` with fields
  add typeSection,
    newObject(
      id = "Lexer",
      parent = "BaseLexer",
      public = true,
      fields = [
        ("kind", tokenKindEnumName),
        ("token", "string"),
        ("error", "string"),
        ("startPos", "int"),
        ("wsno", "int"),
        ("multiLineStr", "bool")
      ]
    )

  # Create `LexerException`
  typeSection.add(
    newObject("LexerException", parent = "CatchableError", public = true))

  let caseOfChar =
    nnkBracketExpr.newTree(
      newDotExpr(ident "lex", ident "buf"),
      newDotExpr(ident "lex", ident "bufpos")
    )
  # Handle {'a'..'z', 'A'..'Z', '_'}
  mainBranches.add((
    cond: nnkCurly.newTree(
      newTree(nnkInfix, ident(".."), newLit('a'), newLit('z')),
      newTree(nnkInfix, ident(".."), newLit('A'), newLit('Z')),
      newLit('_')
    ),
    body: newStmtList(
      newCall(
        ident("handleIdent"),
        ident("lex")
      )
    )
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
        newDotExpr(ident "lex", ident "startPos"),
        newCall(
          newDotExpr(ident "lex", ident "getColNumber"),
          newDotExpr(ident "lex", ident "bufpos"),
        )
      ),
      newAssignment(
        newDotExpr(ident "lex", ident "kind"),
        tok.getIdent(newLit "eof")
      )
    )
  ))

  let caseOfElse =
    newStmtList(
      newCall(
        newDotExpr(ident("lex"), ident("setToken")),
        newDotExpr(
          ident(tokenKindEnumName),
          getIdent(tok, newLit tkUnknown)
        )
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
  if customHandlers.len != 0:
    result.add customHandlers

  # Create `handleIdentCase` compile-time procedure
  let caseOfIdent = newDotExpr(ident "lex", ident "token")
  let identElseBranch = newStmtList(tok.getIdent(newLit tkIdentDefault))
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
  template getTokenBody(allowUnknownTokens: bool, getTkUnknown): untyped =
    lex.startPos = getColNumber(lex, lex.bufpos)
    setLen(lex.token, 0)
    skip lex
    # let tokenVal = lex.buf[lex.bufpos]
    handleMainCase(lex)
    # if not allowUnknownTokens:
    #   if lex.kind == getTkUnknown:
    #     lex.setError("Unexpected token: $1" % [$(tokenVal)])
    result = (
      kind: lex.kind,
      value: lex.token,
      wsno: lex.wsno,
      line: lex.lineNumber,
      col: lex.startPos,
      pos: lex.startPos,
    )

  var tupleConstr = nnkTupleConstr.newTree()
  for tp in [("kind", "kind"), ("value", "token"), ("wsno", "wsno"),
            ("line", "lineNumber"), ("col", "startPos"), ("pos", "startPos")]:
    add tupleConstr,
      nnkExprColonExpr.newTree(
        ident(tp[0]),
        newDotExpr(
          ident("lex"),
          ident(tp[1])
        )
      )

  result.add newProc(
    id = "getToken",
    public = true,
    returnType = ident("TokenTuple"),
    params = [
      ("lex", "Lexer", true)
    ],
    newStmtList(
      newAssignment(
        newDotExpr(ident("lex"), ident("startPos")),
        newCall(ident("getColNumber"), ident("lex"), newDotExpr(ident("lex"), ident("bufpos")))
      ),
      newCall(ident("skip"), ident("lex")),
      newCall(ident("setLen"), newDotExpr(ident("lex"), ident("token")), newLit(0)),
      newCall(ident("handleMainCase"), ident("lex")),
      nnkReturnStmt.newTree(tupleConstr)
    )
  )
  when defined toktokdebug:
    echo result.repr

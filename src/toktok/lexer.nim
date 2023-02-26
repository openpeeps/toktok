# A Generic tokenizer written in Nim language,
# powered by Nim's Macros.
#
# (c) 2022 TokTok is released under MIT License
#          Made by Humans from OpenPeep
#          https://github.com/openpeep/toktok

import std/[streams, macros, strutils, unicode, tables]
import std/lexbase

from std/sequtils import map
from std/algorithm import reversed

export lexbase, streams

var
  tkIdentDefault {.compileTime.} = "Identifier"
  tkPrefix {.compileTime.} = "Tk_"
  tkUnknown {.compileTime.} = "Unknown"
  tkEof {.compileTime.} = "EOF"
  tkInteger {.compileTime.} = "Integer"
  tkFloat {.compileTime.} = "Float"
  tkString {.compileTime.} = "String"
  customTokTokHandlers {.compileTime.} = newStmtList()

include ./macroutils

type
  TType = enum
    TSingle, TRange, TEOL, TVariant

  TK* = ref object
    key: NimNode
    case tokenType: TType
      of TVariant:
        variants: seq[tuple[charNode, key: NimNode, tail: TK]]
      of TRange:
        tail: string
      else: discard
    case valueKind: NimNodeKind
      of nnkCharLit:
        ## Character-based tokenizier.
        ##
        ## ```
        ## Plus     > '+'
        ## Minus    > '-'
        ## Divide   > '/'
        ## Multiply > '*'
        ## ```
        charNode: char
      of nnkStrLit:
        ## All string based tokens based on following case:
        ## ```{'a'..'z', 'A'..'Z', '_'}```
        ##
        ## Use string tokens to tokenize named
        ## identifiers such as:
        ## ```
        ## Function > "function"
        ## Import   > "import"
        ## Class    > "class"
        ## ```
        strv: string
      of nnkCurly:
        ## Sets of strings, allowing for tokenizing
        ## two or more string variations under a single token identifier.
        ## For example:
        ## ```Bool_True > {"True", "true", "TRUE", "yes", "Yes", "YES"}```
        variations: seq[NimNode]
      of nnkCall, nnkCommand:
        ## Optionally, you can pass a custom tokenizer
        tokenizer: tuple[handler: string, charNode: char]
        unicodeTokenizer: tuple[key: string, runes: string]
      else: discard

  CurrentProgram = object
    preferences*: tuple[
      uppercase: bool,
      prefix: string,
      allowUnknown: bool,
      keepUnknownChars: bool,
      handleCustomIdent: bool,
      useUnicode: bool,
    ]
    tokens*: OrderedTable[string, TK]

var Program* {.compileTime.} = CurrentProgram()
var UnicodeTokenKeys {.compileTime.}: seq[string]

proc settings*(program: var CurrentProgram,
              uppercase: bool, prefix = tkPrefix,
              allowUnknown, keepUnknownChars,
              handleCustomIdent, useUnicode = false) {.compileTime.} =
  ## Change toktok settings at compile time using `static` block
  program.preferences = (
    uppercase, prefix, allowUnknown,
    keepUnknownChars, handleCustomIdent,
    useUnicode
  )

proc addToken(tk: NimNode, currToken: TK) {.compileTime.} =
  ## Adds a new token
  Program.tokens[tk.strVal] = currToken

proc getIdent(id: NimNode): NimNode {.compileTime.} =
  ## Returns an identifier
  if Program.preferences.uppercase:
    if Program.preferences.prefix.len != 0:
      return ident toUpperAscii(tkPrefix & id.strVal)
    return ident toUpperAscii(id.strVal)

  if Program.preferences.prefix.len != 0:
    return ident tkPrefix & id.strVal
  result = ident(id.strVal)

proc getIdent(id: string): NimNode {.compileTime.} =
  ## Retrieve an identifier based on given string
  result = getIdent ident(id)

proc getDefaultIdent(): NimNode {.compileTime.} =
  result = tkUnknown.getIdent

proc parseIdentToken(tk: NimNode) {.compileTime.} =
  addToken tk, TK(key: tk.getIdent())

template setInfixToken() =
  if tk[2].kind == nnkCharLit:
    curr.charNode = char(tk[2].intVal)
  elif tk[2].kind == nnkStrLit:
    curr.strv = tk[2].strVal
  elif tk[2].kind == nnkCurly:
    for curlyTk in tk[2]:
      curr.variations.add(curlyTk)

template setInfixTokenRange() =
  if eqIdent(tk[2][0], ".."):
    # handle range-based tokens, for example `#` .. EndOfLine
    if tk[2][2].kind == nnkIdent and tk[2][2].strVal == "EOL":
      curr.tokenType = TType.TEOL
  curr.valueKind = tk[2][1].kind
  if tk[2][1].kind == nnkCharLit:
    # handle char based tokens `!`, `+`
    curr.charNode = char(tk[2][1].intVal)
  elif tk[2][1].kind == nnkStrLit:
    # handle string-based identifiers `a`..`Z`
    curr.strv = tk[2][1].strVal

template setInfixTokenVariants() =
  setInfixToken()
  curr.tokenType = TType.TVariant
  for variant in tk[3]:
    expectKind variant, nnkInfix
    expectKind variant[1], nnkIdent
    let identNode = variant[1]
    if variant[2].kind == nnkCharLit:
      curr.variants.add (
        charNode: variant[2],
        key: getIdent identNode,
        tail: nil
      )
    elif variant[2].kind == nnkInfix:
      if variant[2][0].kind == nnkIdent and variant[2][0].strVal == "..":
        if variant[2][2].kind == nnkIdent:
          # Create a special range object from x to end of line
          if variant[2][2].strVal == "EOL":
            curr.variants.add (
              charNode: variant[2][1],
              key: getIdent identNode,
              tail: TK(tokenType: TEOL)
            )
          else: error("Expected EOL identifier")
        else:
          # Create a TRange object from x to y
          expectKind variant[2][2], nnkStrLit
          curr.variants.add (
            charNode: variant[2][1],
            key: getIdent identNode,
            tail: TK(tokenType: TRange, tail: variant[2][2].strVal)
          )
      else:
        curr.variants.add (
          charNode: variant[2][2],
          key: getIdent identNode,
          tail: nil
        )                    
    elif variant[2].kind == nnkStrLit:
      curr.variants.add (
        charNode: variant[2],
        key: getIdent identNode,
        tail: nil
      )

proc parseInfixToken(tk: NimNode) {.compileTime.} =
  let tkInfixIdent = tk[1]
  var curr = TK(
    key: getIdent tkInfixIdent,
    valueKind: tk[2].kind
  )
  if tk[2].kind in {nnkCall, nnkCommand}:
    if tk[2][0].strVal == "tokenize":
      expectKind tk[2][1], nnkIdent       # a custom identifier
      if tk[2][2].kind != nnkCharLit:
        error("Expected `nnkCharLit` value. $1 given" % [ $(tk[2][2].kind) ])
      curr.tokenizer = (tk[2][1].strVal, char(tk[2][2].intVal))
      setInfixToken()
    elif tk[2][0].strVal == "tokenizeUnicode":
      expectKind tk[2][1], nnkStrLit
      curr.unicodeTokenizer.runes = tk[2][1].strVal
      curr.unicodeTokenizer.key = tkInfixIdent.strVal # keep original token
    else:
      error("Call `tokenize()` for registering custom handlers. Example: `tokenize(myCustomProc, '$')`")
  elif tk.len == 4:
    expectKind tk[3], nnkStmtList
    setInfixTokenVariants()
  elif tk[2].kind == nnkInfix:
    setInfixTokenRange()
  else:
    setInfixToken()
  addToken tk[1], curr

template parseCurrentToken() =
  case tk.kind:
    of nnkIdent:
      parseIdentToken(tk)         # parse token indents without values
    of nnkInfix:
      parseInfixToken(tk)         # parse token identifiers with values or variants
    else: discard

proc createTokenKindEnum(): NimNode {.compileTime.} =
  ## Generates the `TokenKind` enumeration
  var enumTokens: seq[NimNode] = @[]
  enumTokens.add getIdent(tkUnknown)
  enumTokens.add getIdent(tkInteger)
  enumTokens.add getIdent(tkFloat)
  enumTokens.add getIdent(tkString)
  enumTokens.add getIdent(tkIdentDefault)
  enumTokens.add getIdent(tkEof)

  for k, tk in pairs(Program.tokens):
    if tk.tokenType == TType.TVariant:
      for variant in tk.variants:
        enumTokens.add(variant.key)
      enumTokens.add(tk.key)
    elif tk.valueKind == nnkCharLit:
      enumTokens.add(
        nnkEnumFieldDef.newTree(
          tk.key,
          newLit($(tk.charNode))
        )
      )
    elif tk.valueKind == nnkStrLit:
      enumTokens.add(
        nnkEnumFieldDef.newTree(
          tk.key,
          newLit(tk.strv)
        )
      )
    else: enumTokens.add(tk.key)

  result = newEnum(
    ident "TokenKind",
    fields = enumTokens,
    public = true,
    pure = true
  )

proc createCaseStmt(): NimNode =
  var branches: seq[tuple[cond, body: NimNode]]

  for k, tk in pairs(Program.tokens):
    if tk.valueKind == nnkCharLit:
      if tk.tokenType == TType.TSingle:
        branches.add((
          cond: newLit(tk.charNode),
          body: newCall(
            newDotExpr(ident("lex"), ident("setToken")),
            tk.key,
            newLit(1)
          )
        ))
      elif tk.tokenType == TType.TEOL:
        var elifBody = newStmtList()
        elifBody.add(
          newLetStmt(
              ident "toEol",
              newCall(
                ident "nextToEOL",
                ident "lex"
              )
          ),
          newCall(
            ident "setToken", ident "lex",
            tk.key,
            newDotExpr(ident "toEol", ident "pos"),
            newDotExpr(ident "toEol", ident "initPos")
          )
        )
        branches.add((
          cond: newLit(tk.charNode),
          body: elifBody
        ))
      elif tk.tokenType == TType.TVariant:
        var 
          varBranches = nnkIfStmt.newTree()
          allChars = tk.variants
          skipSpecial = false
          i = 1
        for v in reversed(tk.variants):
          inc i
          var strChars: string
          if v.charNode.kind == nnkCharLit and v.tail == nil:
            # Handle char-based variants, for example:
            # of '=':
            #   if next(lex, "="):
            #     setTokenMulti(lex, TK_EQ, 2, 2)
            #   else:
            #     setToken(lex, TK_ASSIGN)
            let chars = map(allChars,
                      proc(x: tuple[charNode, key: NimNode, tail: TK]): string =
                        result = $(char(x.charNode.intVal))
                    )
            discard allChars.pop()
            strChars = chars.join()
          elif v.charNode.kind == nnkCharLit and v.tail != nil:
            # Handle char-based variants with tails
            # of '/':
            #   if next(lex, "/"):
            #       lex.setToken(TK_INLINE_COMMENT, lex.nextToEOL().pos)
            if v.tail.tokenType == TEOL:
              skipSpecial = true
              var elifBody = newStmtList()
              elifBody.add(
                newLetStmt(
                    ident "toEol",
                    newCall(
                      ident "nextToEOL",
                      ident "lex",
                      newLit i,
                    )
                ),
                newCall(
                  ident "setToken", ident "lex",
                  v.key,
                  newDotExpr(ident "toEol", ident "pos"),
                  newDotExpr(ident "toEol", ident "initPos")
                )
              )
              varBranches.add(
                nnkElifBranch.newTree(
                  nnkCall.newTree(
                    ident "next", ident "lex", newLit(char(v.charNode.intVal))
                  ),
                  elifBody
                )
              )
            elif v.tail.tokenType == TRange:
              varBranches.add(
                nnkElifBranch.newTree(
                  nnkCall.newTree(
                    ident "next", ident "lex", newLit(char(v.charNode.intVal))
                  ),
                  newStmtList(
                    nnkCall.newTree(
                      ident "nextToSpec",
                      ident "lex",
                      newLit(v.tail.tail),
                      v.key
                    )
                  )
                )
              )
          else:
            # Handle string-based variants, for example
            # of '@':
            #   if next(lex, "mixin"):
            #     setTokenMulti(lex, TK_MIXIN, 6, 6)
            #   elif next(lex, "include"):
            #     setTokenMulti(lex, TK_INCLUDE, 8, 8)
            #   else:
            #     setToken(lex, TK_AT)
            strChars = v.charNode.strVal
          if not skipSpecial:
            varBranches.add(
              nnkElifBranch.newTree(
                nnkCall.newTree(
                  ident "next", ident "lex", newLit(strChars)
                ),
                newStmtList(
                  nnkCall.newTree(
                    ident "setTokenMulti",
                    ident "lex",
                    v.key,
                    newLit(strChars.len + 1),
                    newLit(strChars.len + 1)
                  )
                )
              )
            )
            skipSpecial = false

        varBranches.add(
          nnkElse.newTree(
            newStmtList(
              newCall(
                ident("setToken"),
                ident("lex"),
                tk.key
              )
            )
          )
        )
        branches.add((
          cond: newLit tk.charNode,
          body: varBranches
        ))
    if tk.valueKind == nnkCall:
      if tk.tokenizer.handler.len != 0:
        branches.add((
          cond: newLit tk.tokenizer.charNode,
          body: newStmtList(
            newCall(
              ident(tk.tokenizer.handler),
              ident("lex"),
              tk.key
            )
          )
        ))
      else:
        UnicodeTokenKeys.add(tk.unicodeTokenizer.key)
    else: continue

  branches.add((
    # {'a'..'z', 'A'..'Z', '_'}
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

  branches.add((
    # {'0'..'9'}
    cond: nnkCurly.newTree(newTree(nnkInfix, ident(".."), newLit('0'), newLit('9'))),
    body: newStmtList(
      newCall(
        ident("handleNumber"),
        ident("lex"),
      )
    )
  ))

  branches.add((
    cond: newLit('\"'),
    body: newStmtList(
      newCall(
        ident("handleString"),
        ident("lex"),
      )
    )
  ))

  branches.add((
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
        getIdent(tkEof)
      )
    )
  ))

  let caseOfIdent =
    nnkBracketExpr.newTree(
      newDotExpr(ident "lex", ident "buf"),
      newDotExpr(ident "lex", ident "bufpos")
    )

  var caseOfElse: NimNode
  if Program.preferences.keepUnknownChars:
    caseOfElse = newStmtList(
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
        getIdent tkUnknown,
        newLit(1)
      ),
    )
  else:
    if Program.preferences.useUnicode:
      if UnicodeTokenKeys.len != 0:
        branches.add((
          cond: newLit('\226'),
          body: newStmtList(
            newIfStmt((
              cond:
                nnkInfix.newTree(
                  ident("=="),
                  nnkBracketExpr.newTree(
                    newDotExpr(
                      ident("lex"),
                      ident("buf")
                    ),
                    nnkInfix.newTree(
                      ident("+"),
                      newDotExpr(
                        ident("lex"),
                        ident("bufpos")
                      ),
                      newLit(1) # next char
                    )
                  ),
                  newLit('\136')
                ),
              body: newStmtList(
                newCall(
                  ident("handleUnicode"),
                  ident("lex")
                )
              )
            ))
          )
        ))
    caseOfElse = newStmtList(
      newCall(
        ident("setToken"),
        ident("lex"),
        getIdent tkUnknown,
        newLit(1)
      )
    )

  newCaseStmt(caseOfIdent, branches, caseOfElse)
proc createStrBasedCaseStmt(): NimNode =
  var branches: seq[tuple[cond, body: NimNode]]
  for k, tk in pairs(Program.tokens):
    if tk.valueKind == nnkStrLit:
      branches.add((
        cond: newLit(tk.strv),
        body: nnkStmtList.newTree(tk.key)
      ))
    elif tk.valueKind == nnkCurly:
      branches.add((
        cond: nnkBracket.newTree(tk.variations),
        body: nnkStmtList.newTree(tk.key)
      ))
    else: continue

  let caseOfIdent = newDotExpr(ident "lex", ident "token")

  let elseBranch =
    if not Program.preferences.handleCustomIdent:
      newStmtList(getIdent(tkIdentDefault))
    else:
      newStmtList(
        newCall(
          newDotExpr(
            ident "lex",
            ident "handleCustomIdent"
          )
        )
      )

  newAssignment(
    newDotExpr(ident "lex", ident "kind"),
    newCaseStmt(caseOfIdent, branches, elseBranch)
  )

macro handlers*(customHandlers) =
  customTokTokHandlers = customHandlers

macro tokens*(tks: untyped) =
  ## Generate tokens based on given identifiers
  ## and create the main `case statement` handler.
  clear(Program.tokens)
  # addToken getDefaultIdent(), TK(key: getDefaultIdent())
  expectKind tks, nnkStmtList
  result = newStmtList()
  for tk in tks:
    parseCurrentToken()
  
  # Create `TokenKind` enumeration
  result.add createTokenKindEnum()

  # Create `TokenTuple` that represents the structure of each 
  # token retrieved via `getToken()` proc
  result.add newTupleType("TokenTuple", public = true, fields = [
    ("kind", "TokenKind"),
    ("value", "string"),
    ("wsno", "int"),
    ("line", "int"),
    ("col", "int"),
    ("pos", "int")
  ])

  # Create `Lexer` object from `BaseLexer` with fields
  result.add newObject(
    id = "Lexer",
    parent = "BaseLexer",
    public = true,
    fields = [
      ("kind", "TokenKind"),
      ("token", "string"),
      ("error", "string"),
      ("startPos", "int"),
      ("wsno", "int"),
      ("multiLineStr", "bool")
    ]
  )

  # Create `LexerException`
  result.add newObject(id = "LexerException", parent = "CatchableError", public = true)

  # Include lexutils file
  result.add newInclude("./lexutils")

  if customTokTokHandlers.len != 0:
    result.add customTokTokHandlers

  # Create `generateIdentCase` compile-time procedure
  result.add newProc(
    id = "generateIdentCase",
    params = [("lex", "Lexer", true)],
    body = createStrBasedCaseStmt()
  )

  # Create `getMainCaseStmt` compile-time procedure
  result.add newProc(
    id = "getMainCaseStmt",
    params = [("lex", "Lexer", true)],
    body = createCaseStmt()
  )

  # Create `getToken` runtime procedure
  template getTokenBody(allowUnknownTokens: bool, getTkUnknown) =
    lex.startPos = lex.getColNumber(lex.bufpos)
    setLen(lex.token, 0)
    skip lex
    let tokenVal = lex.buf[lex.bufpos]
    getMainCaseStmt(lex)
    if not allowUnknownTokens:
      if lex.kind == getTkUnknown:
        lex.setError("Unexpected token: $1" % [$(tokenVal)])
    (
      kind: lex.kind,
      value: lex.token,
      wsno: lex.wsno,
      line: lex.lineNumber,
      col: lex.startPos,
      pos: lex.startPos,
    )

  result.add newProc(
    id = "getToken",
    public = true,
    returnType = ident("TokenTuple"),
    params = [
      ("lex", "Lexer", true)
    ],
    body =
      if Program.preferences.allowUnknown:
        getAst getTokenBody(true.newLit, getIdent(tkUnknown))
      else:
        getAst getTokenBody(false.newLit, getIdent(tkUnknown))
  )

  when defined toktokdebug:
    # compile with -d:toktokdebug to show generated tokens
    echo "------------ Tokens ------------"
    echo indent(createTokenKindEnum().repr, 2)
    
    echo "\n", "------------ Main Case Handler ------------"
    echo indent(createCaseStmt().repr, 2)
    
    echo "\n", "------------ Ident Case Handler ------------"
    echo indent(createStrBasedCaseStmt().repr, 2)
# A Generic tokenizer written in Nim language,
# powered by Nim's Macros.
#
# (c) 2022 TokTok is released under MIT License
#          Made by Humans from OpenPeep
#          https://github.com/openpeep/toktok

import std/[lexbase, streams, macros, tables]
from std/sequtils import map
from std/algorithm import reversed
from std/strutils import `%`, replace, indent, toUpperAscii, startsWith, join
export lexbase, streams

var tkIdentDefault {.compileTime.} = "Identifier"
var tkPrefix {.compileTime.} = "Tk_"
var tkUnknown {.compileTime.} = "Unknown"
var tkEof {.compileTime.} = "Eof"
var tkInteger {.compileTime.} = "Integer"
var tkString {.compileTime.} = "String"

include ./macroutils

type
    TType = enum
        TSingle, TRange, TCharToEndOfLine, TVariant

    CurrentToken* = object
        case tokenType: TType
        of TVariant:
            variants: seq[tuple[charv: char, key: NimNode]]
        else: discard
        key: NimNode
        case valueKind: NimNodeKind
            of nnkCharLit:
                charv: char
            of nnkStrLit:
                strv: string
            else: discard

    CurrentProgram = object
        preferences*: tuple[uppercase: bool, prefix: string]
        tokens*: OrderedTable[string, CurrentToken]

var Program* {.compileTime.} = CurrentProgram()

proc settings*(program: var CurrentProgram, uppercase: bool, prefix = tkPrefix) {.compileTime.} =
    ## Available on compile-time. Use it inside a `static` block to
    ## change your program settings.
    program.preferences = (uppercase, prefix)

proc addToken(tk: NimNode, currToken: CurrentToken) {.compileTime.} =
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
    addToken tk, CurrentToken(key: tk.getIdent())

template setInfixToken() =
    if tk[2].kind == nnkCharLit:
        curr.charv = char(tk[2].intVal)
    elif tk[2].kind == nnkStrLit:
        curr.strv = tk[2].strVal
    elif tk[2].kind == nnkCurly:
        # handle sets todo
        discard

template setInfixTokenRange() =
    if eqIdent(tk[2][0], ".."):
        # handle range-based tokens, for example `#` .. EndOfLine
        if tk[2][2].kind == nnkIdent and tk[2][2].strVal == "EOL":
            curr.tokenType = TType.TCharToEndOfLine
    curr.valueKind = tk[2][1].kind
    if tk[2][1].kind == nnkCharLit:
        # handle char based tokens `!`, `+`
        curr.charv = char(tk[2][1].intVal)
    elif tk[2][1].kind == nnkStrLit:
        # handle string-based identifiers `a`..`Z`
        curr.strv = tk[2][1].strVal

template setInfixTokenVariants() =
    setInfixToken()
    curr.tokenType = TType.TVariant
    for variant in tk[3]:
        expectKind variant, nnkInfix
        expectKind variant[1], nnkIdent
        expectKind variant[2], nnkCharLit
        curr.variants.add (
            charv: char(variant[2].intVal),
            key: getIdent(variant[1])
        )

proc parseInfixToken(tk: NimNode) {.compileTime.} =
    let tkInfixIdent = tk[1]
    let tkInfixVal = tk[2]
    var curr = CurrentToken(
        key: tkInfixIdent.getIdent(),
        valueKind: tk[2].kind
    )

    if tk.len == 4:
        expectKind tk[3], nnkStmtList
        setInfixTokenVariants()
    if tk[2].len == 3:
        setInfixTokenRange()
    else:
        setInfixToken()
    addToken tk[1], curr

template parseToken() =
    case tk.kind:
    of nnkIdent:
        # Parse token identifiers without values
        parseIdentToken(tk)
    of nnkInfix:
        parseInfixToken(tk)
    else: discard

proc createTokenKindEnum(): NimNode {.compileTime.} =
    ## Generates the `TokenKind` enumeration
    var enumTokens: seq[NimNode]
    enumTokens.add getIdent(tkInteger)
    enumTokens.add getIdent(tkString)
    enumTokens.add getIdent(tkIdentDefault)
    enumTokens.add getIdent(tkEof)
    for k, tk in pairs(Program.tokens):
        if tk.tokenType == TType.TVariant:
            for variant in tk.variants:
                enumTokens.add(variant.key)
        enumTokens.add(tk.key)
    result = newEnum(
        ident "TokenKind",
        fields = enumTokens,
        public = true,
        pure = false
    )

proc createCaseStmt(): NimNode =
    ## Create the main `case` statement that
    ## handles all tokens based on their patterns.
    ## ```
    ## case token.value:
    ## of '+': token.kind = TK_PLUS     
    ## ```
    var branches: seq[tuple[cond, body: NimNode]]
    for k, tk in pairs(Program.tokens):
        if tk.valueKind == nnkCharLit:
            if tk.tokenType == TType.TSingle:
                branches.add((
                    cond: newLit(tk.charv),
                    body: newCall(
                        newDotExpr(ident("lex"), ident("setToken")),
                        tk.key,
                        newLit(1)
                    )
                ))
            elif tk.tokenType == TType.TCharToEndOfLine:
                branches.add((
                    cond: newLit(tk.charv),
                    body:
                        nnkCall.newTree(
                            newDotExpr(ident "lex", ident "setToken"),
                            tk.key,
                            newDotExpr(
                                newCall(newDotExpr(ident "lex", ident "nextToEOL")),
                                ident "pos"
                            ),
                        )
                ))
            elif tk.tokenType == TType.TVariant:
                var varBranches = nnkIfStmt.newTree()
                var i = tk.variants.len + 1

                var ifInfix = newEmptyNode()
                var allChars = tk.variants
                for v in reversed(tk.variants):
                    let chars = map(allChars, proc(x: tuple[charv: char, key: NimNode]): string = $x.charv)
                    discard allChars.pop()
                    varBranches.add(
                        nnkElifBranch.newTree(
                            nnkCall.newTree(
                                ident "next", ident "lex", newLit(chars.join())
                            ),
                            newStmtList(
                                nnkCall.newTree(
                                    ident "setTokenMulti",
                                    ident "lex",
                                    v.key,
                                    newLit(chars.len + 1),
                                    newLit(chars.len + 1)
                                )
                            )
                        )
                    )

                varBranches.add(
                    nnkElse.newTree(
                        newStmtList(
                            nnkCall.newTree(
                                ident("setToken"), ident("lex"), tk.key)
                            )
                        )
                )
                branches.add((
                    cond: newLit(tk.charv),
                    body: varBranches
                ))
        else: continue

    branches.add((
        # {'a'..'z', 'A'..'Z', '_'}
        cond: nnkCurly.newTree(
            newTree(nnkInfix, ident(".."), newLit('a'), newLit('z')),
            newTree(nnkInfix, ident(".."), newLit('A'), newLit('Z')),
            newLit('_')
        ),
        body: newStmtList(newCall newDotExpr(ident "lex", ident "handleIdent"))
    ))

    branches.add((
        # {'0'..'9'}
        cond: nnkCurly.newTree(newTree(nnkInfix, ident(".."), newLit('0'), newLit('9'))),
        body: newStmtList(newCall newDotExpr(ident "lex", ident "handleNumber"))
    ))

    branches.add((
        cond: newLit('\''),
        body: newStmtList(newCall newDotExpr(ident "lex", ident "handleString"))
    ))
    branches.add((
        cond: newLit('\"'),
        body: newStmtList(newCall newDotExpr(ident "lex", ident "handleString"))
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

    let caseOfElse = newStmtList(
        newCall(
            newDotExpr(ident("lex"), ident("setToken")),
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
        else: continue

    let caseOfIdent = newDotExpr(ident "lex", ident "token")
    newAssignment(
        newDotExpr(ident "lex", ident "kind"),
        newCaseStmt(caseOfIdent, branches, nnkStmtList.newTree(getIdent(tkIdentDefault)))
    )

macro tokens*(tks: untyped) =
    ## Generate tokens based on given identifiers
    ## and create the main `case statement` handler.
    addToken getDefaultIdent(), CurrentToken(key: getDefaultIdent())
    var tkIdent = getDefaultIdent()

    expectKind tks, nnkStmtList
    result = newStmtList()
    for tk in tks:
        parseToken()
    
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
            ("allowMultilineStrings", "bool")
        ]
    )

    # Create `LexerException`
    result.add newObject(id = "LexerException", parent = "CatchableError", public = true)
    
    # Create `generateIdentCase` compile-time procedure
    result.add newProc(
        id = "generateIdentCase",
        params = [("lex", "Lexer", true)],
        body = createStrBasedCaseStmt()
    )

    # Include lexutils file
    result.add newInclude("./lexutils")

    # Create `getMainCaseStmt` compile-time procedure
    result.add newProc(
        id = "getMainCaseStmt",
        params = [("lex", "Lexer", true)],
        body = createCaseStmt()
    )

    # Create `getToken` runtime procedure
    template getTokenBody() =
        lex.startPos = lex.getColNumber(lex.bufpos)
        setLen(lex.token, 0)
        skip lex
        let tokenVal = lex.buf[lex.bufpos]
        getMainCaseStmt(lex)
        if lex.kind == TK_UNKNOWN:
            lex.setError("Unexpected token: $1" % [$(tokenVal)])
        (
            kind: lex.kind,
            value: lex.token,
            wsno: lex.wsno,
            line: lex.lineNumber,
            col: lex.getColNumber(lex.bufpos),
            pos: lex.startPos,
        )

    result.add newProc(
        id = "getToken",
        public = true,
        returnType = ident("TokenTuple"),
        params = [
            ("lex", "Lexer", true)
        ],
        body = getAst(getTokenBody())
    )

    # echo result.repr

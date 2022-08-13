# A Generic tokenizer written in Nim language,
# powered by Nim's Macros.
#
# (c) 2022 TokTok is released under MIT License
#          Made by Humans from OpenPeep
#          https://github.com/openpeep/toktok

import std/[lexbase, streams, macros, tables]
from std/sequtils import map
from std/algorithm import reversed
from std/strutils import `%`, replace, indent, toUpperAscii, startsWith, join, split
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

    TK* = object
        key: NimNode
        case tokenType: TType
            of TVariant:
                variants: seq[tuple[charNode, key: NimNode]]
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
                ## variations of strings using one token identifier.
                ## For example:
                ## ```Bool_True > {"True", "true", "TRUE", "yes", "Yes", "YES"}```
                variations: seq[NimNode]
            of nnkCall, nnkCommand:
                ## Optionally, you can pass a custom tokenizer
                tokenizer: NimNode
            else: discard

    CurrentProgram = object
        preferences*: tuple[
            uppercase: bool,
            prefix: string
        ]
        tokens*: OrderedTable[string, TK]

var Program* {.compileTime.} = CurrentProgram()

proc settings*(program: var CurrentProgram, uppercase: bool, prefix = tkPrefix) {.compileTime.} =
    ## Available on compile-time. Use it inside a `static` block to
    ## change your program settings.
    program.preferences = (uppercase, prefix)

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
            curr.tokenType = TType.TCharToEndOfLine
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
                key: getIdent identNode
            )
        elif variant[2].kind == nnkInfix:
            curr.variants.add (
                charNode: variant[2][2],
                key: getIdent identNode
            )
        elif variant[2].kind == nnkStrLit:
            curr.variants.add (
                charNode: variant[2],
                key: getIdent identNode
            )
            # echo variant[2].strVal

proc parseInfixToken(tk: NimNode) {.compileTime.} =
    let tkInfixIdent = tk[1]
    let tkInfixVal = tk[2]
    var curr = TK(
        key: getIdent tkInfixIdent,
        valueKind: tk[2].kind
    )

    if tk[2].kind in {nnkCall, nnkCommand}:
        expectKind tk[2][0], nnkIdent
        if tk[2][0].strVal != "tokenize":
            error("Use `tokenize` identifier for registering custom hooks. Example: `tokenize(myCustomProc, '$')`")
        expectKind tk[2][1], nnkIdent       # a custom identifier
        if tk[2][2].kind notin {nnkCharLit, nnkStrLit}:
            error("Custom tokeniziers can only handle `nnkCharLit` or `nnkStrLit`. $1 given" % [ $(tk[2][2].kind) ])
        # curr.tokenizer =
        # echo curr.valueKind
    elif tk.len == 4:
        expectKind tk[3], nnkStmtList
        setInfixTokenVariants()
    elif tk[2].len == 3:
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
            elif tk.tokenType == TType.TCharToEndOfLine:
                branches.add((
                    cond: newLit(tk.charNode),
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
                var i = tk.variants.len + 1 # including the default case

                var ifInfix = newEmptyNode()
                var allChars = tk.variants
                for v in reversed(tk.variants):
                    let chars = map(allChars,
                        proc(x: tuple[charNode, key: NimNode]): string =
                            if x.charNode.kind == nnkCharLit:
                                result = $(char(x.charNode.intVal))
                            elif x.charNode.kind == nnkStrLit:
                                result = x.charNode.strVal
                        )
                    discard allChars.pop()
                    let strChars = chars.join()
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

                varBranches.add(
                    nnkElse.newTree(
                        newStmtList(
                            nnkCall.newTree(
                                ident("setToken"), ident("lex"), tk.key)
                            )
                        )
                )
                branches.add((
                    cond: newLit(tk.charNode),
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
        elif tk.valueKind == nnkCurly:
            branches.add((
                cond: nnkBracket.newTree(tk.variations),
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
    clear(Program.tokens)
    addToken getDefaultIdent(), TK(key: getDefaultIdent())
    var tkIdent = getDefaultIdent()

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
            ("multilineStrings", "bool")
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

# TokTok, Generic tokenizer written in Nim language. Powered by Nim's Macros.
#
# (c) 2021 TokTok is released under MIT License
#          George Lemon | Made by Humans from OpenPeep
#          https://github.com/openpeep/toktok

import std/[lexbase, streams, macros, tables]
from std/strutils import `%`, replace, indent, toUpperAscii, startsWith

export lexbase, streams

type
    StringTuple = tuple[strToken: string, tokToken: string]
    CharTuple = tuple[charToken: char, tokToken: string]
    SeqStringTuple  = seq[StringTuple]
    SeqCharTuple    = seq[CharTuple]
    SeqStrTupleEOS = seq[tuple[rangeStart: StringTuple, rangeEnd: StringTuple]]
    SeqCharTupleEOS = seq[tuple[rangeStart: CharTuple, rangeEnd: CharTuple]]

let
    lexer_object_ident {.compileTime.} = "Lexer"
    lexer_object_inherit {.compileTime.} = "BaseLexer"
    lexer_param_ident {.compileTime.} = "lex"

    enum_token_ident {.compileTime.} = "TokenKind"
    token_tuple_ident {.compileTime.} = "TokenTuple"
    tkPrefix {.compileTime.} = newLit("tk_")
    tkUnknown {.compileTime.} = "TK_UNKNOWN"
    tkEOF {.compileTime.} = "TK_EOF"
    tkInt {.compileTime.} = "TK_INTEGER"
    tkString {.compileTime.} = "TK_STRING"

var 
    tkIdentifier {.compileTime.} = "TK_IDENTIFIER"
    prefIncludeWhitespaces {.compileTime.} = false
    prefPromptTokens {.compileTime.} = false
    prefPrefixTokens {.compileTime.} = "TK_"
    prefUppercaseTokens {.compileTime.} = true

macro toktokSettings*(
    includeWhitespaces,
    uppercaseTokens = true,
    promptTokens = false,
    prefix = "TK_",
    defaultTkIdent = "TK_IDENTIFIER"
) =
    prefPromptTokens = promptTokens.boolVal
        # whether to list tokens on compile time via cli
    prefIncludeWhitespaces = includeWhitespaces.boolVal
        # tokenize whitespaces or count as integer
    prefPrefixTokens = prefix.strVal
        # add a prefix 
    prefUppercaseTokens = uppercaseTokens.boolVal
        # transform tokens to uppercaseAscii
    tkIdentifier = defaultTkIdent.strVal

# dumpAstGen:
#     case ok:
#     of '@':
#         if next(lex, 'a') and next(lex, 'b') and next(lex, 'c'):
#             setToken(lex, TK_IMPORT, 2)

macro tokens*(tks: untyped) =
    ## Generate TokenKind enumeration based on given identifiers and keys.
    ## Keys can be `int`, `char` or `string` and are used for creating
    ## the main `case statement` of your lexer
    tks.expectKind(nnkStmtList)
    if prefPromptTokens == true:
        echo "✨ Compiling toktok...\n"

    result = nnkStmtList.newTree()

    var enumTokensNode = newNimNode(nnkEnumTy)
    # var caseTokens = newNimNode(nnkCaseStmt)
    var
        caseStrTokens:      SeqStringTuple
        caseStrTokensEOL:   SeqStringTuple
        caseCharTokens:     SeqCharTuple
        caseCharTokensEOL:  SeqCharTuple

        caseStrTokensEOS: SeqStrTupleEOS
        caseCharTokensEOS: SeqCharTupleEOS

    # var caseRangeTokens: seq[tuple[charStart: char, charEnd: char, tokToken: string]]
    enumTokensNode.add(newEmptyNode())
    
    # add TK_UNKNOWN at the begining of TokenKind enum
    # add TK_INTEGER as second
    var tkIdent = ident(toUpperAscii(tkUnknown))
    enumTokensNode.add(tkIdent)
    enumTokensNode.add(ident(toUpperAscii(tkInt)))
    enumTokensNode.add(ident(toUpperAscii(tkString)))
    
    var specialIdents: seq[tuple[chartk: char, tok: string]]
    var variants: Table[char, seq[tuple[chartk: seq[char], tok: string]]]

    for tk in tks:
        if tk.kind == nnkIdent:
            # Handle token names without values
            tkIdent = ident(toUpperAscii(tkPrefix.strVal & tk.strVal))
            enumTokensNode.add(tkIdent)
        elif tk.kind == nnkInfix:
            if tk.len == 4:
                # Handle variants of tokens
                tk[3].expectKind(nnkStmtList)
                for infixVariant in tk[3]:
                    infixVariant[0].expectKind(nnkIdent)
                    infixVariant[1].expectKind(nnkIdent)
                    echo infixVariant[1].strVal
                # for id in tk[3][0]:
                #     if id.kind == nnkIdent:
                #         echo id.strVal

                # for tkVariantIdent in tk[3][0][2]:
                #     echo char(tkVariantIdent.intVal)

            tkIdent = ident(toUpperAscii(tkPrefix.strVal & tk[1].strVal))
            enumTokensNode.add(tkIdent)
            if tk[2].kind == nnkStrLit:
                # handle string based tokens
                if prefPromptTokens == true:
                    echo "\n  Token:", indent(tk[1].strVal, 7), "\n  Keyword:", indent(tk[2].strVal, 5)
                caseStrTokens.add((strToken: tk[2].strVal, tokToken: tk[1].strVal))
            elif tk[2].kind == nnkPrefix:
                if tk[2][1].kind == nnkBracket:
                    # handle string or char based tokens for alternative keywords
                    for altKey in tk[2][1]:
                        if altKey.kind == nnkStrLit:
                            caseStrTokens.add((strToken: altKey.strVal, tokToken: tk[1].strVal))
                        else:
                            caseCharTokens.add((charToken: char(altKey.intVal), tokToken: tk[1].strVal))
                            # echo altKey.kind
            elif tk[2].kind == nnkInfix:
                # let infixStr = tk[2][0].strVal
                if tk[2][0].strVal == "..":
                    if tk[2][2].kind == nnkIdent:
                        if tk[2][2].strVal == "EOL":
                            # Collect all chars from given start to end of line
                            if tk[2][1].kind == nnkStrLit:
                                caseStrTokensEOL.add((strToken: tk[2][1].strVal, tokToken: tk[1].strVal))
                            else:
                                caseCharTokensEOL.add((charToken: char(tk[2][1].intVal), tokToken: tk[1].strVal))
                    elif tk[2][2].kind == nnkCharLit:
                        ## Collect all chars from specified start char to end char
                        if tk[2][1].kind == nnkStrLit:
                            caseStrTokensEOS.add (
                                    rangeStart: (strToken: tk[2][1].strVal, tokToken: tk[1].strVal),
                                    rangeEnd: (strToken: tk[2][2].strVal, tokToken: tk[1].strVal)
                                )
                        elif tk[2][1].kind == nnkCharLit:
                            caseCharTokensEOS.add (
                                    rangeStart: (charToken: char(tk[2][1].intVal), tokToken: tk[1].strVal),
                                    rangeEnd: (charToken: char(tk[2][2].intVal), tokToken: tk[1].strVal)
                                )
                        else: discard # TODO raise error
            else:
                let tokToken = tk[1].strVal
                if tk[2].kind == nnkTupleConstr:
                    # handle variant based tokens for chars and strings like
                    # if next == '!' and next == '=': TK_MY_TOKEN
                    var variantCase: seq[char]
                    let firstChar = char(tk[2][0].intVal)
                    for variant in tk[2]:
                        if variant.kind == nnkCharLit:
                            variantCase.add char(variant.intVal)
                        elif variant.kind == nnkStrLit:
                            for v in variant.strVal:
                                variantCase.add v
                        if not variants.hasKey(firstChar):
                            variants[firstChar] = newSeq[tuple[chartk: seq[char], tok: string]]()
                    variants[firstChar].add (chartk: variantCase, tok: tokToken)
                elif tk[2].kind == nnkCharLit:
                    let charToken = char(tk[2].intval)
                    if prefPromptTokens == true:
                        echo "\n  Token:", indent(tokToken, 7), "\n  Keyword:", indent($charToken, 5)
                    caseCharTokens.add((charToken: charToken, tokToken: tokToken))
                elif tk[2].kind == nnkCall:
                    if tk[2][0].strVal == "identWith":
                        specialIdents.add (chartk: char(tk[2][1].intVal), tok: tokToken)
                else: discard # TODO raise error
        else: discard   # TODO raise error

    # add TK_EOF at the end
    tkIdent = ident(toUpperAscii(tkEOF))
    enumTokensNode.add(tkIdent)

    tkIdent = ident(toUpperAscii(tkIdentifier))
    enumTokensNode.add(tkIdent)

    # Creates a public `TokenKind* = enum` with all given tokens
    result.add(
        newNimNode(nnkTypeSection).add(
            newNimNode(nnkTypeDef).add(
                newNimNode(nnkPostfix).add(
                    ident "*",
                    ident enum_token_ident
                ),
                newEmptyNode(),
                enumTokensNode
            )
        ),
        newNimNode(nnkTypeSection).add(
            newNimNode(nnkTypeDef).add(
                newNimNode(nnkPostfix).add(
                    ident "*",
                    ident "LexerException"
                ),
                newEmptyNode(),
                newNimNode(nnkObjectTy).add(
                    newEmptyNode(),
                    nnkOfInherit.newTree(
                        ident "CatchableError"
                    ),
                    newEmptyNode()
                )
            )
        )
    )

    # Create TokenTuple
    # TokenTuple = tuple[kind: TokenKind, value: string, wsno, col, line: int]
    result.add(
        newNimNode(nnkTypeSection).add(
            newNimNode(nnkTypeDef).add(
                newNimNode(nnkPostfix).add(
                    ident "*",
                    ident token_tuple_ident
                ),
                newEmptyNode(),
                newNimNode(nnkTupleTy).add(
                    newNimNode(nnkIdentDefs).add(
                        ident "kind",
                        ident enum_token_ident,
                        newEmptyNode()
                    ),
                    newNimNode(nnkIdentDefs).add(
                        ident "value",
                        ident "string",
                        newEmptyNode()
                    ),
                    newNimNode(nnkIdentDefs).add(
                        ident "wsno",
                        ident "col",
                        ident "line",
                        ident "int",
                        newEmptyNode()
                    ),
                )
            )
        )
    )

    # Create Token = object
    var fields = @[
        (key: "kind", fType: enum_token_ident),
        (key: "token", fType: "string"),
        (key: "error", fType: "string"),
        (key: "startPos", fType: "int"),
        (key: "wsno", fType: "int"),
        (key: "allowMultilineStrings", ftype: "bool")
    ]

    var objectFields = nnkRecList.newTree()
    for f in fields:
        objectFields.add(
            newNimNode(nnkIdentDefs).add(
                ident f.key,
                ident f.fType,
                newEmptyNode()
            )
        )

    result.add(
        nnkTypeSection.newTree(
            nnkTypeDef.newTree(
                nnkPostfix.newTree(
                    ident "*",
                    ident lexer_object_ident
                ),
                newEmptyNode(),
                nnkObjectTy.newTree(
                    newEmptyNode(),
                    nnkOfInherit.newTree(ident lexer_object_inherit),
                    objectFields
                )
            )
        )
    )

    result.add(
        nnkIncludeStmt.newTree(
            nnkInfix.newTree(
                ident "/",
                ident "toktok",
                ident "lexutils"
            )
        )
    )

    # Start creation of Case Statement, and add the first case
    # case lex.buf[lex.bufpos]:
    var mainCaseStatements = newNimNode(nnkCaseStmt)
    mainCaseStatements.add(
        nnkBracketExpr.newTree(
            nnkDotExpr.newTree(
                ident(lexer_param_ident),
                ident("buf")
            ),
            nnkDotExpr.newTree(
                ident(lexer_param_ident),
                ident("bufpos")
            )
        )
    )

    # of EndOfFile:
    #   lex.startPos = lex.getColNumber(lex.bufpos)
    #   lex.kind = TK_EOF
    mainCaseStatements.add(
        nnkOfBranch.newTree(
            ident "EndOfFile",
            newStmtList(
                newAssignment(
                    newDotExpr(ident(lexer_param_ident), ident("startPos")),
                    newCall(
                        newDotExpr(ident(lexer_param_ident), ident("getColNumber")),
                        newDotExpr(ident(lexer_param_ident), ident("bufpos")),
                    )
                ),
                newAssignment(
                    newDotExpr(ident(lexer_param_ident), ident("kind")),
                    ident("TK_EOF")
                )
            )
        )
    )

    # Tokenize from given start char to EOL
    for caseCharTokEOL in caseCharTokensEOL:
        let tokTokenChar = toUpperAscii(tkPrefix.strVal & caseCharTokEOL.tokToken)
        mainCaseStatements.add(
            newNimNode(nnkOfBranch).add(
                newLit(caseCharTokEOL.charToken),
                nnkStmtList.newTree(
                    nnkCall.newTree(
                        nnkDotExpr.newTree(
                            ident(lexer_param_ident),
                            ident("setToken")
                        ),
                        ident(tokTokenChar),
                        nnkDotExpr.newTree(
                            nnkCall.newTree(
                                nnkDotExpr.newTree(
                                    ident(lexer_param_ident),
                                    ident("nextToEOL")
                                )
                            ),
                            ident("pos")
                        )
                    )
                )
            )
        )

    # Tokenize from given start char to end char
    # of 'A':
    #   lex.nextToSpec(endChar = 'B', tokenKind: TK_RANGE_EXAMPLE)
    #   
    # TODO ``nextToSpec`` procedure should be able to handle multi lines
    # between `A` and `B`
    for caseCharTokEOS in caseCharTokensEOS:
        let endChar = toUpperAscii(tkPrefix.strVal & caseCharTokEOS.rangeEnd.tokToken)
        mainCaseStatements.add(
            nnkOfBranch.newTree(
                newLit caseCharTokEOS.rangeStart.charToken,
                newStmtList(
                    newCall(
                        newDotExpr(ident lexer_param_ident, ident "nextToSpec"),
                        newLit caseCharTokEOS.rangeEnd.charToken,
                        ident endChar
                    ),
                )
            )
        )

    # Define case statements for string-based identifiers
    # This case is triggered via handleIdent() template from lexutils,
    var strBasedCaseStatement = newNimNode(nnkCaseStmt)
    strBasedCaseStatement.add(
        newNimNode(nnkDotExpr).add(
            ident(lexer_param_ident),
            ident("token")
        )
    )
    for caseStr in caseStrTokens:
        let tokTokenStr = toUpperAscii(tkPrefix.strVal & caseStr.tokToken)
        strBasedCaseStatement.add(
            newNimNode(nnkOfBranch).add(
                newLit(caseStr.strToken),
                newNimNode(nnkStmtList).add(ident(tokTokenStr))
            )
        )
    
    strBasedCaseStatement.add(
        newNimNode(nnkElse).add(
            newNimNode(nnkStmtList).add(tkIdent)
        )
    )

    # Create `generateIdentCase()` template and define
    # case for string-based tokens
    var identCaseTemplate = newNimNode(nnkTemplateDef)
    identCaseTemplate.add(
        newNimNode(nnkPostfix).add(
            ident "*",
            ident "generateIdentCase"
        ),
        newEmptyNode(),
        newNimNode(nnkGenericParams).add(
            newNimNode(nnkIdentDefs).add(
                ident "L",
                ident "Lexer",
                newEmptyNode()
            )
        ),
        nnkFormalParams.newTree(
            newEmptyNode(),
            nnkIdentDefs.newTree(
                ident lexer_param_ident,
                newNimNode(nnkVarTy).add(
                    ident "L"
                ),
                newEmptyNode()
            )
        ),
        newEmptyNode(),
        newEmptyNode(),
        newStmtList(
            newAssignment(
                newDotExpr(ident lexer_param_ident, ident "kind"),
                strBasedCaseStatement
            )
        )
    )

    # create template generateIdentCase*() =
    result.add(identCaseTemplate)

    # Define case for integers (0..9)
    # and use handleNumber() template from lexutils
    mainCaseStatements.add(
        newNimNode(nnkOfBranch).add(
            newNimNode(nnkInfix).add(
                ident(".."),
                newLit('0'),
                newLit('9')
            ),
            newStmtList(newCall(newDotExpr(ident lexer_param_ident, ident "handleNumber")))
        )
    )

    mainCaseStatements.add(
        newNimNode(nnkOfBranch).add(
            newLit('\"'),
            newLit('\''),
            newStmtList(newCall(newDotExpr(ident lexer_param_ident, ident "handleString")))
        )
    )


    # Define case for a-z-A-Z 
    # and use handleIdent() template from lexutils
    mainCaseStatements.add(
        newNimNode(nnkOfBranch).add(
            newNimNode(nnkInfix).add(
                ident(".."),
                newLit('a'),
                newLit('z')
            ),
            newNimNode(nnkInfix).add(
                ident(".."),
                newLit('A'),
                newLit('Z')
            ),
            newStmtList(newCall(newDotExpr(ident(lexer_param_ident), ident "handleIdent")))
        )
    )
    
    # Add to Main Case Statement char-based tokens
    for caseChar in caseCharTokens:
        let tokTokenStr = toUpperAscii(tkPrefix.strVal & caseChar.tokToken)
        var charVariants = nnkStmtList.newTree()
        # echo variants
        if variants.hasKey(caseChar.charToken):
            var variantConditional = nnkIfStmt.newTree()
            for currentVariant in mitems(variants[caseChar.charToken]):
                var chartok: string
                var charsetstring: string
                currentVariant.chartk.delete(0) # delete first char as we already have it as ``caseChar``
                for currChar in currentVariant.chartk:
                    # collect char by char and create a string
                    # to check if next token is as ``lex.next("mystring")``
                    charsetstring.add currChar
                variantConditional.add(
                    nnkElifBranch.newTree(
                        newCall(ident "next", ident "lex", newLit(charsetstring)),
                        newStmtList(
                            newCall(
                                ident "setToken",
                                ident "lex",
                                ident toUpperAscii(tkPrefix.strVal & currentVariant.tok),
                                newLit(charsetstring.len + 1)
                            )
                        )
                    )
                )
            variants.del(caseChar.charToken) # delete from variants Table
            variantConditional.add(
                nnkElse.newTree(
                    newCall(
                        newDotExpr(ident lexer_param_ident, ident "setToken"),
                        ident tokTokenStr,
                        newLit(1)                           # char token offset in lex.bufpos
                    )
                )
            )
            charVariants.add(variantConditional)
            mainCaseStatements.add(
                nnkOfBranch.newTree(
                    newLit(caseChar.charToken),
                    charVariants
                )
            )
        else:
            mainCaseStatements.add(
                nnkOfBranch.newTree(
                    newLit(caseChar.charToken),
                    newCall(
                        newDotExpr(ident lexer_param_ident, ident "setToken"),
                        ident tokTokenStr,
                        newLit(1)                           # char token offset in lex.bufpos
                    )
                )
            )

    if specialIdents.len != 0:
        for specialIdent in specialIdents:
            mainCaseStatements.add(
                nnkOfBranch.newTree(
                    newLit(specialIdent.chartk),
                    newCall(
                        newDotExpr(ident lexer_param_ident, ident "handleIdentWith"),
                        ident toUpperAscii(tkPrefix.strVal & specialIdent.tok)
                    )
                )
            )

    if variants.len != 0:
        ## add the rest of variants
        var charVariants = nnkStmtList.newTree()
        var variantConditional = nnkIfStmt.newTree()
        for charKey, variant in mpairs(variants):
            var chartok: string
            var charsetstring: string
            for currCharSet in mitems(variant):
                currCharSet.chartk.delete(0)
                chartok = currCharSet.tok
                for currChar in currCharSet.chartk:
                    charsetstring.add currChar
            variantConditional.add(
                nnkElifBranch.newTree(
                    newCall(ident "next", ident "lex", newLit(charsetstring)),
                    newStmtList(
                        newCall(
                            ident "setToken",
                            ident "lex",
                            ident toUpperAscii(tkPrefix.strVal & chartok),
                            newLit(charsetstring.len + 1),
                        )
                    )
                )
            )
            charVariants.add(variantConditional)
            mainCaseStatements.add(
                nnkOfBranch.newTree(
                    newLit(charKey),
                    charVariants
                )
            )
        variants.clear()
    
    mainCaseStatements.add(
        nnkElse.newTree(
            newStmtList(
                newCall(
                    ident "setToken",
                    ident "lex",
                    ident tkUnknown
                )
            )
        )
    )

    # Create a public procedure that retrieves token one by one.
    # This proc should be used while you parse for tokens
    # proc getToken*[T: Lexer](lex: var T): TokenTuple =
    result.add(
        nnkProcDef.newTree(
            nnkPostfix.newTree(
                ident "*",
                ident "getToken"
            ),
            newEmptyNode(),
            nnkGenericParams.newTree(
                nnkIdentDefs.newTree(
                    ident "T",
                    ident "Lexer",
                    newEmptyNode()
                )
            ),
            nnkFormalParams.newTree(
                ident token_tuple_ident,
                nnkIdentDefs.newTree(
                    ident lexer_param_ident,
                    nnkVarTy.newTree(
                        ident "T"
                    ),
                    newEmptyNode()
                )
            ),
            newEmptyNode(),
            newEmptyNode(),
            newStmtList(
                # lex.startPos = lex.getColNumber(lex.bufpos)
                newAssignment(
                    newDotExpr(ident lexer_param_ident, ident "kind"),
                    ident(tkUnknown)
                ),
                # setLen(lex.token, 0)
                newCall(
                    ident "setLen",
                    newDotExpr(ident lexer_param_ident, ident "token"),
                    newLit(0)
                ),
                nnkCommand.newTree(
                    ident "skip",
                    ident lexer_param_ident
                ),

                # Unpack collected case statements for char, string, and int-based tokens
                mainCaseStatements,

                nnkIfStmt.newTree(
                    nnkElifBranch.newTree(
                        nnkInfix.newTree(
                            ident("=="),
                            newDotExpr(ident "lex", ident "kind"),
                            ident(tkUnknown)
                        ),
                        newStmtList(
                            newCall(
                                newDotExpr(ident "lex", ident "setError"),
                                newLit("Unrecognized character") # throws ``Unrecognized char`` for unknown tokens
                            )
                        )
                    )
                ),
                newAssignment(
                    ident "result",
                    nnkTupleConstr.newTree(
                        nnkExprColonExpr.newTree(
                            ident "kind",
                            newDotExpr(ident lexer_param_ident, ident "kind")
                        ),
                        nnkExprColonExpr.newTree(
                            ident "value",
                            newDotExpr(ident lexer_param_ident, ident "token")
                        ),
                        nnkExprColonExpr.newTree(
                            ident "wsno",
                            newDotExpr(ident lexer_param_ident, ident "wsno")
                        ),
                        nnkExprColonExpr.newTree(
                            ident "col",
                            newDotExpr(ident lexer_param_ident, ident "startPos")
                        ),
                        nnkExprColonExpr.newTree(
                            ident "line",
                            newDotExpr(ident lexer_param_ident, ident "lineNumber")
                        )
                    )
                )
            )
        )
    )

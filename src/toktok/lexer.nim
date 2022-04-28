import std/[lexbase, streams, macros]
from std/strutils import `%`, replace, indent, toUpperAscii, startsWith
from std/sequtils import toSeq

export lexbase, streams

type
    SeqStringTuple  = seq[tuple[strToken: string, tokToken: string]]
    SeqCharTuple    = seq[tuple[charToken: char, tokToken: string]]
    SeqStrTupleEOS[T: tuple[strToken: string, tokToken: string]]  = seq[tuple[rangeStart: T, rangeEnd: T]]
    SeqCharTupleEOS[T: tuple[charToken: char, tokToken: string]] = seq[tuple[rangeStart: T, rangeEnd: T]]

let
    lexer_object_ident {.compileTime.} = "Lexer"
    lexer_object_inherit {.compileTime.} = "BaseLexer"
    lexer_param_ident {.compileTime.} = "lex"
    lexer_exception_ident {.compileTime.} = "LexerException"

    template_hasError_ident {.compileTime.} = newLit("hasError")
    proc_getError_ident {.compileTime.} = newLit("getError")

    enum_token_ident {.compileTime.} = newLit("TokenKind")
    token_tuple_ident {.compileTime.} = newLit("TokenTuple")
    tkPrefix {.compileTime.} = newLit("tk_")
    tkUnknown {.compileTime.} = newLit("TK_UNKNOWN")
    tkIdentifier {.compileTime.} = newLit("TK_IDENTIFIER")
    tkEOF {.compileTime.} = newLit("TK_EOF")
    tkInt {.compileTime.} = newLit("Tk_Integer")
    tkString {.compileTime.} = "Tk_String"

var 
    prefIncludeWhitespaces {.compileTime.} = false
    prefPromptTokens {.compileTime.} = false
    prefPrefixTokens {.compileTime.} = "TK_"
    prefUppercaseTokens {.compileTime.} = true

macro toktokSettings*(
    includeWhitespaces, promptTokens, uppercaseTokens: static bool,
    prefixTokens: static string) =
    # whether to list tokens on compile time via cli
    prefPromptTokens = promptTokens
    # tokenize whitespaces or count as integer
    prefIncludeWhitespaces = includeWhitespaces
    # add a prefix 
    prefPrefixTokens = prefixTokens
    # transform tokens to uppercaseAscii
    prefUppercaseTokens = uppercaseTokens

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
    var tkIdent = newIdentNode(toUpperAscii(tkUnknown.strVal))
    enumTokensNode.add(tkIdent)
    enumTokensNode.add(newIdentNode(toUpperAscii(tkInt.strVal)))
    enumTokensNode.add(newIdentNode(toUpperAscii(tkString)))
    
    for tk in tks:
        # tk.expectKind(nnkIdent)
        if tk.kind == nnkIdent:
            tkIdent = newIdentNode(toUpperAscii(tkPrefix.strVal & tk.strVal))
            enumTokensNode.add(tkIdent)
        elif tk.kind == nnkInfix:
            tkIdent = newIdentNode(toUpperAscii(tkPrefix.strVal & tk[1].strVal))
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
                            caseCharTokens.add((charToken: char(altKey.intval), tokToken: tk[1].strVal))
                            # echo altKey.kind
            elif tk[2].kind == nnkInfix:
                let infixStr = tk[2][0].strVal
                if tk[2][0].strVal == "..":
                    if tk[2][2].kind == nnkIdent:
                        if tk[2][2].strVal == "EOL":
                            # Collect all chars from given start to end of line
                            if tk[2][1].kind == nnkStrLit:
                                caseStrTokensEOL.add((strToken: tk[2][1].strVal, tokToken: tk[1].strVal))
                            else:
                                caseCharTokensEOL.add((charToken: char(tk[2][1].intVal), tokToken: tk[1].strVal))
                    elif tk[2][2].kind == nnkCharLit:
                        ## Collect all chars from given start point to specified end point
                        if tk[2][1].kind == nnkStrLit:
                            caseStrTokensEOS.add((
                                rangeStart: (strToken: tk[2][1].strVal, tokToken: tk[1].strVal),
                                rangeEnd: (strToken: tk[2][2].strVal, tokToken: tk[1].strVal)
                            ))
                        elif tk[2][1].kind == nnkCharLit:
                            caseCharTokensEOS.add((
                                rangeStart: (charToken: char(tk[2][1].intVal), tokToken: tk[1].strVal),
                                rangeEnd: (charToken: char(tk[2][2].intVal), tokToken: tk[1].strVal)
                            ))
                        else: discard # TODO raise error

            else: # Collect all char-based cases
                let charToken = char(tk[2].intval)
                let tokToken = tk[1].strVal
                if prefPromptTokens == true:
                    echo "\n  Token:", indent(tokToken, 7), "\n  Keyword:", indent($charToken, 5)
                caseCharTokens.add((charToken: charToken, tokToken: tokToken))
        else: discard   # TODO raise error

    # add TK_EOF at the end
    tkIdent = newIdentNode(toUpperAscii(tkEOF.strVal))
    enumTokensNode.add(tkIdent)

    tkIdent = newIdentNode(toUpperAscii(tkIdentifier.strVal))
    enumTokensNode.add(tkIdent)

    # echo caseCharTokensEOS

    # Creates a public `TokenKind* = enum` with all given tokens
    result.add(
        newNimNode(nnkTypeSection).add(
            newNimNode(nnkTypeDef).add(
                newNimNode(nnkPostfix).add(
                    newIdentNode("*"),
                    newIdentNode(enum_token_ident.strVal)
                ),
                newEmptyNode(),
                enumTokensNode
            )
        )
    )
    
    # LexerException object
    # LexerException = object of CatchableError
    result.add(
        newNimNode(nnkTypeSection).add(
            newNimNode(nnkTypeDef).add(
                newNimNode(nnkPostfix).add(
                    newIdentNode("*"),
                    newIdentNode(lexer_exception_ident)
                ),
                newEmptyNode(),
                newNimNode(nnkObjectTy).add(
                    newEmptyNode(),
                    newNimNode(nnkOfInherit).add(newIdentNode("CatchableError")),
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
                    newIdentNode("*"),
                    newIdentNode(token_tuple_ident.strval)
                ),
                newEmptyNode(),
                newNimNode(nnkTupleTy).add(
                    newNimNode(nnkIdentDefs).add(
                        newIdentNode("kind"),
                        newIdentNode(enum_token_ident.strVal),
                        newEmptyNode()
                    ),
                    newNimNode(nnkIdentDefs).add(
                        newIdentNode("value"),
                        newIdentNode("string"),
                        newEmptyNode()
                    ),
                    newNimNode(nnkIdentDefs).add(
                        newIdentNode("wsno"),
                        newIdentNode("col"),
                        newIdentNode("line"),
                        newIdentNode("int"),
                        newEmptyNode()
                    ),
                )
            )
        )
    )

    # Create Token = object
    var fields = @[
        (key: "kind", fType: enum_token_ident.strval),
        (key: "token", fType: "string"),
        (key: "error", fType: "string"),
        (key: "startPos", fType: "int"),
        (key: "wsno", fType: "int"),
    ]

    var objectFields = newNimNode(nnkRecList)
    for f in fields:
        objectFields.add(
            newNimNode(nnkIdentDefs).add(
                newIdentNode(f.key),
                newIdentNode(f.fType),
                newEmptyNode()
            )
        )

    result.add(
        newNimNode(nnkTypeSection).add(
            newNimNode(nnkTypeDef).add(
                newNimNode(nnkPostfix).add(
                    newIdentNode("*"),
                    newIdentNode(lexer_object_ident)
                ),
                newEmptyNode(),
                newNimNode(nnkObjectTy).add(
                    newEmptyNode(),
                    newNimNode(nnkOfInherit).add(newIdentNode(lexer_object_inherit)),
                    objectFields
                )
            )
        )
    )

    result.add(
        nnkIncludeStmt.newTree(
            nnkInfix.newTree(
                newIdentNode("/"),
                newIdentNode("toktok"),
                newIdentNode("lexutils")
            )
        )
    )

    # Start creation of Case Statement, and add the first case
    # case lex.buf[lex.bufpos]:
    var mainCaseStatements = newNimNode(nnkCaseStmt)
    mainCaseStatements.add(
        nnkBracketExpr.newTree(
            nnkDotExpr.newTree(
                newIdentNode(lexer_param_ident),
                newIdentNode("buf")
            ),
            nnkDotExpr.newTree(
                newIdentNode(lexer_param_ident),
                newIdentNode("bufpos")
            )
        )
    )

    # of EndOfFile:
    #   lex.startPos = lex.getColNumber(lex.bufpos)
    #   lex.kind = TK_EOF
    mainCaseStatements.add(
        nnkOfBranch.newTree(
            newIdentNode("EndOfFile"),
            nnkStmtList.newTree(
                nnkAsgn.newTree(
                    nnkDotExpr.newTree(
                        newIdentNode(lexer_param_ident),
                        newIdentNode("startPos")
                    ),
                    nnkCall.newTree(
                        nnkDotExpr.newTree(
                            newIdentNode(lexer_param_ident),
                            newIdentNode("getColNumber")
                        ),
                        nnkDotExpr.newTree(
                            newIdentNode(lexer_param_ident),
                            newIdentNode("bufpos")
                        )
                    )
                ),
                nnkAsgn.newTree(
                    nnkDotExpr.newTree(
                        newIdentNode(lexer_param_ident),
                        newIdentNode("kind")
                    ),
                    newIdentNode("TK_EOF")
                )
            )
        )
    )

    # Handle tokenizier from start to end of line
    for caseCharTokEOL in caseCharTokensEOL:
        let tokTokenChar = toUpperAscii(tkPrefix.strVal & caseCharTokEOL.tokToken)
        mainCaseStatements.add(
            newNimNode(nnkOfBranch).add(
                newLit(caseCharTokEOL.charToken),
                nnkStmtList.newTree(
                    nnkCall.newTree(
                        nnkDotExpr.newTree(
                            newIdentNode(lexer_param_ident),
                            newIdentNode("setToken")
                        ),
                        newIdentNode(tokTokenChar),
                        nnkDotExpr.newTree(
                            nnkCall.newTree(
                                nnkDotExpr.newTree(
                                    newIdentNode(lexer_param_ident),
                                    newIdentNode("nextToEOL")
                                )
                            ),
                            newIdentNode("pos")
                        )
                    )
                )
            )
        )

    # Define case statements for string-based identifiers
    # This case is triggered via handleIdent() template from lexutils,
    var strBasedCaseStatement = newNimNode(nnkCaseStmt)
    strBasedCaseStatement.add(
        newNimNode(nnkDotExpr).add(
            newIdentNode(lexer_param_ident),
            newIdentNode("token")
        )
    )
    for caseStr in caseStrTokens:
        let tokTokenStr = toUpperAscii(tkPrefix.strVal & caseStr.tokToken)
        strBasedCaseStatement.add(
            newNimNode(nnkOfBranch).add(
                newLit(caseStr.strToken),
                newNimNode(nnkStmtList).add(newIdentNode(tokTokenStr))
            )
        )
    
    strBasedCaseStatement.add(
        newNimNode(nnkElse).add(
            newNimNode(nnkStmtList).add(newIdentNode("TK_IDENTIFIER"))
        )
    )

    # Create `generateIdentCase()` template and define
    # case for string-based tokens
    var identCaseTemplate = newNimNode(nnkTemplateDef)
    identCaseTemplate.add(
        newNimNode(nnkPostfix).add(
            newIdentNode("*"),
            newIdentNode("generateIdentCase")
        ),
        newEmptyNode(),
        newNimNode(nnkGenericParams).add(
            newNimNode(nnkIdentDefs).add(
                newIdentNode("L"),
                newIdentNode("Lexer"),
                newEmptyNode()
            )
        ),
        newNimNode(nnkFormalParams).add(
            newEmptyNode(),
            newNimNode(nnkIdentDefs).add(
                newIdentNode(lexer_param_ident),
                newNimNode(nnkVarTy).add(
                    newIdentNode("L")
                ),
                newEmptyNode()
            )
        ),
        newEmptyNode(),
        newEmptyNode(),
        newNimNode(nnkStmtList).add(
            newNimNode(nnkAsgn).add(
                newNimNode(nnkDotExpr).add(
                    newIdentNode(lexer_param_ident),
                    newIdentNode("kind")
                ),
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
                newIdentNode(".."),
                newLit('0'),
                newLit('9')
            ),
            newNimNode(nnkStmtList).add(
                newNimNode(nnkCall).add(
                    newNimNode(nnkDotExpr).add(
                        newIdentNode(lexer_param_ident),       # TODO, replace string with compileTime var
                        newIdentNode("handleNumber")           # TODO, replace string with compileTime var
                    )
                )
            )
        )
    )

    mainCaseStatements.add(
        newNimNode(nnkOfBranch).add(
            newLit('\"'),
            newLit('\''),
            newNimNode(nnkStmtList).add(
                newNimNode(nnkCall).add(
                    newNimNode(nnkDotExpr).add(
                        newIdentNode(lexer_param_ident),
                        newIdentNode("handleString")
                    )
                )
            )
        )
    )


    # Define case for a-z-A-Z 
    # and use handleIdent() template from lexutils
    mainCaseStatements.add(
        newNimNode(nnkOfBranch).add(
            newNimNode(nnkInfix).add(
                newIdentNode(".."),
                newLit('a'),
                newLit('z')
            ),
            newNimNode(nnkInfix).add(
                newIdentNode(".."),
                newLit('A'),
                newLit('Z')
            ),
            newNimNode(nnkStmtList).add(
                newNimNode(nnkCall).add(
                    newNimNode(nnkDotExpr).add(
                        newIdentNode(lexer_param_ident),
                        newIdentNode("handleIdent")
                    )
                )
            )
        )
    )

    # Add to Main Case Statement char-based tokens
    for caseChar in caseCharTokens:
        let tokTokenStr = toUpperAscii(tkPrefix.strVal & caseChar.tokToken)
        mainCaseStatements.add(
            newNimNode(nnkOfBranch).add(
                newLit(caseChar.charToken),
                newNimNode(nnkCall).add(
                    newNimNode(nnkDotExpr).add(
                        newIdentNode(lexer_param_ident),
                        newIdentNode("setToken")
                    ),
                    newIdentNode(tokTokenStr),
                    newLit(1)                           # char token offset in lex.bufpos
                )
            )
        )

    mainCaseStatements.add(
        newNimNode(nnkElse).add(
            nnkStmtList.newTree(
                nnkAsgn.newTree(
                    nnkDotExpr.newTree(
                        newIdentNode(lexer_param_ident),
                        newIdentNode("kind")
                    ),
                    newIdentNode("TK_IDENTIFIER")       # TODO, replace string with compileTime var
                )
            )
        )
    )

    # Create a public procedure that retrieves token one by one.
    # This proc should be used in the main while iteration inside your parser:
    # proc getToken*[T: Lexer](lex: var T): TokenTuple =
    result.add(
        nnkProcDef.newTree(
            nnkPostfix.newTree(
                newIdentNode("*"),
                newIdentNode("getToken")
            ),
            newEmptyNode(),
            nnkGenericParams.newTree(
                nnkIdentDefs.newTree(
                    newIdentNode("T"),
                    newIdentNode("Lexer"),
                    newEmptyNode()
                )
            ),
            nnkFormalParams.newTree(
                newIdentNode(token_tuple_ident.strVal),
                nnkIdentDefs.newTree(
                    newIdentNode(lexer_param_ident),
                    nnkVarTy.newTree(
                        newIdentNode("T")
                    ),
                    newEmptyNode()
                )
            ),
            newEmptyNode(),
            newEmptyNode(),
            nnkStmtList.newTree(
                # lex.startPos = lex.getColNumber(lex.bufpos)
                nnkAsgn.newTree(
                    nnkDotExpr.newTree(
                        newIdentNode(lexer_param_ident),
                        newIdentNode("kind")
                    ),
                    newIdentNode("TK_UNKNOWN")
                ),
                # setLen(lex.token, 0)
                nnkCall.newTree(
                    newIdentNode("setLen"),
                    nnkDotExpr.newTree(
                        newIdentNode(lexer_param_ident),
                        newIdentNode("token")
                    ),
                    newLit(0)
                ),
                nnkCommand.newTree(
                    newIdentNode("skip"),
                    newIdentNode(lexer_param_ident)
                ),

                # Unpack collected case statements
                # for char, string and int-based tokens
                mainCaseStatements,
                
                # TODO, create nnkAsgn dynamically
                nnkAsgn.newTree(
                    newIdentNode("result"),
                    nnkTupleConstr.newTree(
                        nnkExprColonExpr.newTree(
                            newIdentNode("kind"),
                            nnkDotExpr.newTree(
                                newIdentNode(lexer_param_ident),
                                newIdentNode("kind")
                            )
                        ),
                        nnkExprColonExpr.newTree(
                            newIdentNode("value"),
                            nnkDotExpr.newTree(
                                newIdentNode(lexer_param_ident),
                                newIdentNode("token")
                            )
                        ),
                        nnkExprColonExpr.newTree(
                            newIdentNode("wsno"),
                            nnkDotExpr.newTree(
                                newIdentNode(lexer_param_ident),
                                newIdentNode("wsno")
                            )
                        ),
                        nnkExprColonExpr.newTree(
                            newIdentNode("col"),
                            nnkDotExpr.newTree(
                                newIdentNode(lexer_param_ident),
                                newIdentNode("startPos")
                            )
                        ),
                        nnkExprColonExpr.newTree(
                            newIdentNode("line"),
                            nnkDotExpr.newTree(
                                newIdentNode(lexer_param_ident),
                                newIdentNode("lineNumber")
                            )
                        )
                    )
                )
            )
        )
    )

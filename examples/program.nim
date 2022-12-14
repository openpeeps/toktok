import std/[json, jsonutils]
import toktok

from strutils import `%`, parseInt, parseBool
from std/enumutils import symbolName

static:
    Program.settings(
        uppercase = true,
        prefix = "Tk_"
    )

tokens:
    Plus      > '+'
    Minus     > '-'
    Multi     > '*'
    GT           > '>':
        GTE      ? '='
    LT           > '<':
        LTE      ? '='
    Not          > '!':
        NEQ      ? '='
    Assign    > '=':
        EQ > '='
    Div       > '/':
        Block_Comment ? '*' .. "*/"
        Inline_Comment ? '/' .. EOL
    Let       > "let"
    Const     > "const"
    Var       > "var"
    SetTrue   > {"TRUE", "true"}
    SetFalse  > {"FALSE", "false"}
    If        > "if"
    Elif      > "elif"
    Else      > "else"
    LCUR      > '{'
    RCUR      > '}'
    LPAR      > '('
    RPAR      > ')'

# AST API
type
    NType = enum
        NTInt
        NTString
        NTBool
        NTCondition
        NTInlineComment
        NTBlockComment
        NTInfix
        NTStmtList

    OperatorType* {.pure.} = enum
        None
        EQ          = "=="
        NE          = "!="
        GT          = ">"
        GTE         = ">="
        LT          = "<"
        LTE         = "<="

    IfBranch* = tuple[cond: Node, body: seq[Node]]
    ElifBranch* = seq[IfBranch]

    Node = ref object
        nodeName: string        # a string symbol of nodeType
        case nodeType: NType
        of NTInt:
            intVal: int
        of NTString:
            strVal: string
        of NTBool:
            boolVal: bool
        of NTInfix:
            infixOp: OperatorType
            infixOpSymbol: string
            infixLeft, infixRight*: Node
        of NTCondition:
            ifCond: Node
            ifBody, elseBody: Node
            elifBranch: ElifBranch
        of NTStmtList:
            stmtList: seq[Node]
        of NTInlineComment, NTBlockComment:
            comment: string

    Program* = object
        nodes: seq[Node]

proc getOpType(tk: TokenKind): OperatorType =
    case tk:
    of TK_EQ: result = EQ
    of TK_NEQ: result = NE
    of TK_LT: result = LT
    of TK_LTE: result = LTE
    of TK_GT: result = GT
    of TK_GTE: result = GTE
    else: discard

proc newStmtList(exp: seq[Node]): Node =
    result = Node(nodeName: NTStmtList.symbolName, nodeType: NTStmtList, stmtList: exp)        

proc newInlineComment(comment: string): Node =
    result = Node(nodeName: NTInlineComment.symbolName, nodeType: NTInlineComment, comment: comment)

proc newBlockComment(comment: string): Node =
    result = Node(nodeName: NTBlockComment.symbolName, nodeType: NTBlockComment, comment: comment)

# Parser API
type
    Parser* = object
        lex: Lexer
        prev, current, next: TokenTuple
        statements: Program
        error: string

    PrefixFunction = proc(p: var Parser): Node

const tkOperators = {TK_EQ, TK_NEQ, TK_LT, TK_LTE, TK_GT, TK_GTE}

# defer
proc getPrefixFn(p: var Parser, kind: TokenKind): PrefixFunction
proc parseStatement(p: var Parser): Node
proc parseExpression(p: var Parser): Node

proc setError(p: var Parser, msg: string) =
    ## Set parser error
    p.error = "Error ($2:$3): $1" % [msg, $p.current.line, $p.current.pos]

proc hasError(p: var Parser): bool {.inline.} =
    p.error.len != 0

proc walk(p: var Parser, offset = 1) =
    var i = 0
    while offset != i:
        p.prev = p.current
        p.current = p.next
        p.next = p.lex.getToken()
        inc i

proc parseInteger(p: var Parser): Node =
    result = Node(nodeName: NTString.symbolName, nodeType: NTInt, intVal: parseInt(p.current.value))
    walk(p)

proc parseString(p: var Parser): Node =
    result = Node(nodeName: NTString.symbolName, nodeType: NTString, strVal: p.current.value)
    walk(p)

proc parseInfix(p: var Parser): Node =
    var leftNode, rightNode: Node
    var leftFn, rightFn: PrefixFunction

    leftFn = p.getPrefixFn(p.current.kind)
    if leftFn != nil:
        leftNode = leftFn(p)
    
    if p.current.kind notin tkOperators:
        p.setError("Invalid operator")
        return
    let opType = getOpType(p.current.kind)
    walk(p)

    rightFn = p.getPrefixFn(p.current.kind)
    if rightFn != nil:
        rightNode = rightFn(p)

    result = Node(
        nodeName:       NTInfix.symbolName,
        nodeType:       NTInfix,
        infixOpSymbol:  opType.symbolName,
        infixLeft:      leftNode,
        infixOp:        opType,
        infixRight:     rightNode
    )

proc parseCondition(p: var Parser): Node =
    let this = p.current
    walk(p) # skip `if`
    let infixNode: Node = p.parseInfix()
    if p.current.kind != TK_LCUR:
        p.setError("Missing \"{\" after conditional statement")
        return
    walk(p) # skip {
    if infixNode != nil:
        result = Node(nodeName: NTCondition.symbolName, nodeType: NTCondition)
        result.ifCond = infixNode
        var ifBodyNodes: seq[Node]
        while p.current.kind != TK_RCUR:
            if p.current.kind == TK_EOF:
                p.setError("EOF reached before closing condition body")
                break
            elif p.current.pos <= this.pos:
                p.setError("Missing \"}\" after condition body")
                break
            ifBodyNodes.add(p.parseExpression())
        if p.hasError(): return
        walk(p) # }
        result.ifBody = newStmtList(ifBodyNodes)

proc unexpected(p: var Parser): Node =
    ## TODO, show chars value in current TokenTuple
    p.setError("Unexpected token \"$1\"" % [$p.current.kind])
    return nil

proc parseInlineComment(p: var Parser): Node =
    result = newInlineComment(p.current.value)
    walk(p)

proc parseBlockComment(p: var Parser): Node =
    result = newBlockComment(p.current.value)
    walk(p)

proc getPrefixFn(p: var Parser, kind: TokenKind): PrefixFunction =
    result = case kind:
        of TK_INTEGER: parseInteger
        of TK_STRING: parseString
        of TK_IF: parseCondition
        of TK_INLINE_COMMENT: parseInlineComment
        of TK_BLOCK_COMMENT: parseBlockComment
        else: unexpected

proc parseExpression(p: var Parser): Node =
    let this = p.current
    var callPrefixFn = p.getPrefixFn(this.kind)
    let exp: Node = p.callPrefixFn()
    if exp == nil: return
    result = exp

proc parseStatement(p: var Parser): Node =
    let exp = p.parseExpression()
    if exp == nil or p.hasError: return
    result = newStmtList(@[exp])

proc parseProgram(p: var Parser): Node =
    result = p.parseExpression()

proc getStatementsStr(p: Parser): string =
    return pretty(toJson(p.statements))

when isMainModule:
    var p = Parser(
        lex: Lexer.init(fileContents = readFile("program.txt")),
        statements: Program()
    )

    p.current = p.lex.getToken()
    p.next = p.lex.getToken()

    while p.current.kind != TK_EOF and p.error.len == 0:
        var statement: Node = p.parseProgram()
        if statement != nil:
            p.statements.nodes.add(statement)

    if p.error.len != 0:
        echo p.error
    else:
        echo p.getStatementsStr()
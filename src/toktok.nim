# TokTok, Generic tokenizer written in Nim language. Powered by Nim's Macros.
#
# (c) 2021 TokTok is released under MIT License
#          George Lemon | Made by Humans from OpenPeep
#          https://github.com/openpeep/toktok

import toktok/lexer
export lexer

when isMainModule:
    static:
        Program.settings(
            uppercase = true,
            prefix = "Tk_"
        )

    tokens:
        Plus      > '+'
        Minus     > '-'
        Multi     > '*'
        Div       > '/':
            BlockComment ? '*' .. "*/"
            InlineComment ? '/' .. EOL
        Let       > "let"
        Const     > "const"
        Var       > "var"
        SetTrue   > {"TRUE", "True", "true"}
        SetFalse  > {"FALSE", "False", "false"}

    var lex = Lexer.init(fileContents = readFile("example.txt"))
    if lex.hasError:
        echo lex.getError
    else:
        while true:
            var curr = lex.getToken()           # tuple[kind: TokenKind, value: string, wsno, col, line: int]
            if curr.kind == TK_EOF: break
            echo curr
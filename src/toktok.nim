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
        Div       > '/'
        Assign    > '='
        Comment   > '#' .. EOL      # anything from `#` to end of line
        CommentAlt > "/*" .. "*/"   # anything starting with `/*` to `*/`
        Var       > "var"
        Let       > "let"
        Const     > "const"
        # single case for multi keywords
        BTrue     > @["TRUE", "True", "true", "YES", "Yes", "yes", "y"]
        BFalse    > @["FALSE", "False", "false", "NO", "No", "no", "n"]

    var lex = Lexer.init(fileContents = "const hello = 1 + 1")
    if lex.hasError:
        echo lex.getError
    else:
        while true:
            var curr = lex.getToken()           # tuple[kind: TokenKind, value: string, wsno, col, line: int]
            if curr.kind == TK_EOF: break
            echo curr
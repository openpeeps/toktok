# TokTok, Generic tokenizer written in Nim language. Powered by Nim's Macros.
#
# (c) 2021 TokTok is released under MIT License
#          George Lemon | Made by Humans from OpenPeep
#          https://github.com/openpeep/toktok

import toktok/lexer
export lexer

when isMainModule:
    static:
        Program.settings(true, "Tk_")

    tokens:
        # Plus       > '+'
        # Minus      > '-'
        # Multi      > '*'
        Not       > '!':
            Neq      > '='
            Sneq     > '='
        Comment    > '#' .. EOL
        # Assign     > '='
        # String     > '"' @ handleString
        # Var        > "var"
        # Let        > "let"
        # Const      > "const"
        # Class      > "class"
        # Bool_True  > @["TRUE", "True", "true", "YES", "Yes", "yes", "y"]
        # Bool_False > @["FALSE", "False", "false", "NO", "No", "no", "n"]

    var lex = Lexer.init(readFile("sample"))
    var current: TokenTuple
    while not lex.hasError:
        current = lex.getToken()
        if current.kind == TK_EOF: break
        echo current

    if lex.hasError:
        echo lex.getError()
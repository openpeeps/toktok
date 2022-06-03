# TokTok, Generic tokenizer written in Nim language. Powered by Nim's Macros.
#
# (c) 2021 TokTok is released under MIT License
#          George Lemon | Made by Humans from OpenPeep
#          https://github.com/openpeep/toktok

import toktok/lexer
export lexer

when isMainModule:
    const sample = """
const hello = 1 + 1
"""
    tokens:
        Plus       > '+'
        Minus      > '-'
        Multi      > '*'
        Div        > '/'
        Assign     > '='
        Comment    > '#' .. EOL
        Var        > "var"
        Let        > "let"
        Const      > "const"
        Bool_True  > @["TRUE", "True", "true", "YES", "Yes", "yes", "y"]
        Bool_False > @["FALSE", "False", "false", "NO", "No", "no", "n"]

    var lex = Lexer.init(sample)
    var current = lex.getToken()
    while not lex.hasError:
        current = lex.getToken()
        if current.kind == TK_EOF: break
        echo current
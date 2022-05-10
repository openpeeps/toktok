# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest
import toktok

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

test "correct welcome":
    var lex = Lexer.init(sample)
    check lex.getToken().kind == TK_CONST
    check lex.getToken().kind == TK_IDENTIFIER
    check lex.getToken().kind == TK_ASSIGN
    check lex.getToken().kind == TK_INTEGER
    check lex.getToken().kind == TK_PLUS
    check lex.getToken().kind == TK_INTEGER

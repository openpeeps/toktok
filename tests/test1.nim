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
registerTokens defaultSettings:
    plus     = '+'
    minus    = '-'
    multi    = '*'
    `div`    = '/'
    assign   = '='
    comment  = '#' .. EOL
    `var`    = "var"
    `let`    = "let"
    `const`  = "const"
    `bool`   = ["true", "false"]

test "correct welcome":
    var lex = newLexer(sample)
    check lex.getToken().kind == tkConst
    check lex.getToken().kind == tkIdentifier
    check lex.getToken().kind == tkAssign
    check lex.getToken().kind == tkInteger
    check lex.getToken().kind == tkPlus
    check lex.getToken().kind == tkInteger

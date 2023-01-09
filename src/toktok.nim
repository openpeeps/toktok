# TokTok, Generic tokenizer written in Nim language. Powered by Nim's Macros.
#
# (c) 2021 TokTok is released under MIT License
#          George Lemon | Made by Humans from OpenPeep
#          https://github.com/openpeep/toktok

import toktok/lexer
export lexer

# let sampleCode = """
# 100.000
# """

# when isMainModule:
#     static:
#         Program.settings(
#             uppercase = true,
#             prefix = "Tk_"
#         )

#     tokens:
#         Plus      > '+'
#         Minus     > '-'
#         Multi     > '*'
#         Assign    > '=':
#             EQ > '='
#         Div       > '/':
#             BlockComment ? '*' .. "*/"
#             InlineComment ? '/' .. EOL
#         Let       > "let"
#         Const     > "const"
#         Var       > "var"

#     var lex = Lexer.init(fileContents = sampleCode)
#     if lex.hasError:
#         echo lex.getError
#     else:
#         var curr = lex.getToken()
#         while curr.kind != TK_EOF:
#             if lex.hasError:
#                 echo lex.getError
#                 break
#             echo curr
#             curr = lex.getToken()
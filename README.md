<p align="center">
    <img src="https://raw.githubusercontent.com/openpeep/toktok/main/.github/logo.png" width="140px"><br>
    Generic tokenizer written in Nim language, powered by Nim's Macros 👑   
</p>

<p align="center">
  <code>nimble install toktok</code>
</p>

<p align="center">
  <a href="https://openpeeps.github.io/toktok">API reference</a> <br><br>
  <img src="https://github.com/openpeeps/toktok/workflows/test/badge.svg" alt="Github Actions">  <img src="https://github.com/openpeeps/toktok/workflows/docs/badge.svg" alt="Github Actions">
</p>


## 😍 Key Features
- [x] Support `char`, `string`, `int` based tokens
- [x] Powered by Nim's Macros ✨
- [x] No RegEx 🍃
- [x] Meta-programming `TokenKind*` `enum`
- [x] Meta-programming `case` statement for all tokens
- [x] `getToken()` procedure to retrieve token by token
- [ ] Multi `TokenKind` handler in one line (`=`, `==`, `!==`)
- [x] Open Source | `MIT`

Toktok is a generic Lexer, based on standard Nim libraries `streams`, `lexbase` and `macros`.
It is meant to be used by higher level parsers for writing any kind of tools or programs.

## Debug
Compile with `-d:toktokdebug` to print your tokens

## Quick Example

<details>
    <summary>Show sample.txt example</summary>

```
const hello = 1 + 1
```

</details>

```nim
import toktok

static:
  Program.settings(
    uppercase = true,
    prefix = "Tk_",
    allowUnknown = false,
    keepUnknownChars = false
  )

tokens:
  Plus      > '+'
  Minus     > '-'
  Multi     > '*'
  Div       > '/':
    BlockComment ? '*' .. "*/"
    InlineComment ? '/' .. EOL
  Assign      > '='
  Var         > "var"
  Let         > "let"
  Const       > "const"
  SetTrue     > {"TRUE", "True", "true", "YES", "Yes", "yes", "y"}
  SetFalse    > {"FALSE", "False", "false", "NO", "No", "no", "n"}

when isMainModule:
  var lex = Lexer.init(fileContents = readFile("sample.txt"))
  if lex.hasError:
    echo lex.getError
  else:
    while true:
      var curr = lex.getToken()           # tuple[kind: TokenKind, value: string, wsno, col, line: int]
      if curr.kind == TK_EOF: break
      echo curr
```

<details>
    <summary>See output</summary>

```nim
(kind: TK_CONST, value: "const", wsno: 1, col: 0, line: 1)
(kind: TK_IDENTIFIER, value: "hello", wsno: 1, col: 6, line: 1)
(kind: TK_ASSIGN, value: "", wsno: 0, col: 12, line: 1)
(kind: TK_INTEGER, value: "1", wsno: 1, col: 15, line: 1)
(kind: TK_PLUS, value: "", wsno: 0, col: 16, line: 1)
(kind: TK_INTEGER, value: "1", wsno: 1, col: 19, line: 1)
```

</details>

<a href="https://hetzner.cloud/?ref=Hm0mYGM9NxZ4"><img src="https://openpeep.ro/banners/openpeep-footer.png" width="100%"></a>

### ❤ Contributions & Support
- 🐛 Found a bug? [Create a new Issue](https://github.com/openpeeps/toktok/issues)
- 👋 Wanna help? [Fork it!](https://github.com/openpeeps/toktok/fork)
- 😎 [Get €20 in cloud credits from Hetzner](https://hetzner.cloud/?ref=Hm0mYGM9NxZ4)
- 🥰 [Donate to The Enthusiast via PayPal address](https://www.paypal.com/donate/?hosted_button_id=RJK3ZTDWPL55C)

### 🎩 License
`toktok` | `MIT` license. [Made by Humans from OpenPeeps](https://github.com/openpeeps).<br>
Copyright &copy; 2023 OpenPeeps & Contributors &mdash; All rights reserved.

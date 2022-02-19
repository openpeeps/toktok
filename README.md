<p align="center">
    <img src=".github/logo.png" width="140px"><br>
    Generic tokenizer written in Nim language, powered by Nim's Macros 👑 (WIP)
</p>

## 😍 Key Features
- [x] Support `char`, `string`, `int` based tokens
- [x] Powered by Nim's Macro
- [ ] Multi `TokenKind` handler in one line (`=`, `==`, `!==`)
- [ ] Available as a Native NodeJS addon (soon)
- [x] Open Source | `MIT`

## Installing
```bash
nimble install toktok
```

## Quick Example

<details>
    <summary>Show sample.txt example</summary>

```
const hello = 1 + 1
```

</details>

```nim
import toktok

# this is optional
toktokSettings(
    includeWhitespaces = false,         # if set true, each whitespace is tokenized as TK_WS
    promptTokens = true,                # list all tokens on compile time
    uppercaseTokens = true,             # transform tokens to uppercase, for example `Plus` > `TK_PLUS`
    prefixTokens = "TK_"                # set a prefix for all tokens (default `TK_`)
)

tokens:
    Plus      > '+'
    Minus     > '-'
    Multi     > '*'
    Div       > '/'
    Var       > "var"
    Let       > "let"
    Const     > "const"

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
(kind: TK_CONST, value: "const", wsno: 0, col: 0, line: 1)
(kind: TK_IDENTIFIER, value: "hello", wsno: 0, col: 0, line: 1)
(kind: TK_INTEGER, value: "1", wsno: 0, col: 0, line: 1)
(kind: TK_PLUS, value: "+", wsno: 0, col: 0, line: 1)
(kind: TK_INTEGER, value: "1", wsno: 0, col: 0, line: 1)
```

</details>

### Tips
Toktok can handle integers for `'0'..'9'` and identifiers for `'a'..'z', 'A'..'Z'`


## Based on Toktok
Here you can find some cool projects based on Toktok Lexer

| | | |
| ---- | ---- | ---- |
| <a href="https://github.com/openpeep/tim"><img src="https://raw.githubusercontent.com/openpeep/tim/main/.github/tim.png" width="115px"></a> | <a href="https://github.com/psypac/psypac"><img src="https://raw.githubusercontent.com/psypac/psypac/main/.github/psypac.png" width="115px"></a> | <a href="https://github.com/openpeep/parrot"><img src="https://raw.githubusercontent.com/openpeep/parrot/main/.github/parrot-logo.png" width="115px"></a> |

_Your Toktok project here_

## Roadmap

#### 0.1.0
- [ ] Add tests

### 0.2.0
_todo_

### ❤ Contributions
If you like this project you can contribute to Toktok by opening new issues, fixing bugs, contribute with code, ideas and you can even [donate via PayPal address](https://www.paypal.com/donate/?hosted_button_id=RJK3ZTDWPL55C) 🥰

### 👑 Discover Nim language
<strong>What's Nim?</strong> Nim is a statically typed compiled systems programming language. It combines successful concepts from mature languages like Python, Ada and Modula. [Find out more about Nim language](https://nim-lang.org/)

<strong>Why Nim?</strong> Performance, fast compilation and C-like freedom. We want to keep code clean, readable, concise, and close to our intention. Also a very good language to learn in 2022.

### 🎩 License
Toktok is an Open Source Software released under `MIT` license. [Developed by Humans from OpenPeep](https://github.com/openpeep).<br>
Copyright &copy; 2022 OpenPeep & Contributors &mdash; All rights reserved.

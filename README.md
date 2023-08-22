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
- ✨ Powered by Nim's Macros
- 🪄 Based on `std/lexbase` / Zero Regular Expression
- Compile-time generation using macro-based **TokenKind** `enum`, `lexbase`
- Runtime generation using **TokenKind** `tables`, `lexbase`
- Open Source | `MIT`

> [!NOTE]
> This is a generic Lexer, based on std/ `streams`, `lexbase` and `macros`. It is meant to be used by higher level parsers for writing any kind of tools or programs.

> [!NOTE]
> Compile with `-d:toktokdebug` to inspect the generated code.

## Quick Example

```nim
# Register your tokens
registerTokens defaultSettings:
  `const` = "const"
  `echo` = "hello"
  asgn = '=':   # `=` is tkAsgn
    eq = '='    # `==` is tkEQ
  excl = '!':
    ne = '='
  at = '@':
    import = tokenizer(handleImport, "import") 


# Tokenizing...
var
  tok = lexer.init(sample)
  prev: TokenTuple
  curr: TokenTuple = tok.getToken
  next: TokenTuple = tok.getToken 

proc walk(tok: var Lexer) =
  prev = curr
  curr = next
  next = tok.getToken

while likely(curr.kind != tkEOF):
  if tok.hasError: break
  echo curr # use `getToken` consumer to get token by token
  walk tok
```

## TODO
- [ ] Runtime Token generation using tables/critbits

### ❤ Contributions & Support
- 🐛 Found a bug? [Create a new Issue](https://github.com/openpeeps/toktok/issues)
- 👋 Wanna help? [Fork it!](https://github.com/openpeeps/toktok/fork)
- 😎 [Get €20 in cloud credits from Hetzner](https://hetzner.cloud/?ref=Hm0mYGM9NxZ4)
- 🥰 [Donate to OpenPeeps via PayPal address](https://www.paypal.com/donate/?hosted_button_id=RJK3ZTDWPL55C)

### 🎩 License
`MIT` license. [Made by Humans from OpenPeeps](https://github.com/openpeeps).<br>
Copyright &copy; 2023 OpenPeeps & Contributors &mdash; All rights reserved.

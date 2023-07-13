# Package

version       = "0.1.2"
author        = "George Lemon"
description   = "Generic tokenizer written in Nim language 👑 Powered by Nim's Macros"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 1.4.0"

task tests, "Run test":
  exec "testament p 'tests/*.nim'"

task docgen, "Generate API documentation":
  exec "nim doc --project --index:on --outdir:htmldocs src/toktok.nim"
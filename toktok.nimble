# Package

version       = "0.1.0"
author        = "George Lemon"
description   = "Generic tokenizer written in Nim language 👑 Powered by Nim's Macros"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 1.4.0"

task tests, "Run test":
    exec "testament p 'tests/*.nim'"
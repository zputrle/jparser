# Diving into combinatiry parsing

This project is my attempt to work through [The "Understanding Parser Combinators" series](https://eli.thegreenplace.net/2017/deciphering-haskells-applicative-and-monadic-parsers/) by Ken Lamug, to understand what combinatory parsing is, how it works and how it can be applied.

As a result I have created a small library that can be used to build parsers through a technique called combinatory parsing and demonstrated it's use by building "JSON-like parser" - a JSON parser without a support for Unicode characters.
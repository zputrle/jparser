# JParser

**JParser** is a JSON parser build from scratch using a technique called combinatory parsing.

This project is my attempt to work through a series of articles ["Understanding Parser Combinators"](https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/) written by **Scott Wlaschin**, and was created mainly for me to **experiment** with the basic ideas of combinatory parsing. Therefore, while writing the code, I preferred readability over optimization and I em sure that I did not cover all the edge cases that come with trying to build a full-blown JSON parser.

## How to use it

You can find JParser in `src/jparser.ml`. The code is self contained and does not use any external dependencies (except for the OCaml's standard library). The parser only supports UTF-8 encoded string.

For an example on how to use JParser, see `examples/example.ml`. The example can be build by calling `make example`.

``` bash
$ make example
ocamlbuild -I src -I utils examples/example.native
Finished, 10 targets (10 cached) in 00:00:00.
./example.native

{
        patient_name : "John Doe",
        patient_id : "231143",
        sex : "M",
        age : 31.
}
```

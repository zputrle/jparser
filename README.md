# JParser

**JParser** is a JSON parser build from scratch using the combinatory parsing technique.

This project is my attempt to work through a series of articles ["Understanding Parser Combinators"](https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/) written by **Scott Wlaschin**, and was created mainly for me to _experiment_ with the basic ideas of combinatory parsing. Therefore, while writing the code, I preferred readability over optimization and I am sure that I did not cover all the edge cases that come with trying to build a full-blown JSON parser.

## How to use

You can find JParser in `src/jparser.ml`. The code is self-contained and does not use any external dependencies (except for the OCaml's standard library). The parser only supports UTF-8 encoded strings.

The example of how to use JParser can be found in `examples/example.ml`. You can buld it build by calling `make example`.

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

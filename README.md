# Blam Language
This is a hobby language called `blam`. It is intended as a learning project, to learn about language development.
It is heavily inspired by [Crafting Interpreters](https://craftinginterpreters.com), the [wren](https://wren.io/) language, [Writing A Compiler In Go](https://compilerbook.com/). It uses a [stack based vm](https://en.wikipedia.org/wiki/Stack_machine).

# Features (all of these are WIP)
- Class based
- Single inheritance
- first class functions
- closures
- stack based vm
- expression based `return if (x == 1) { 23; } else { 4; };`, `var fn = fun(a) { return a + 1; }`
- errors as values
- progressively typed

# Running
```console
git clone --depth=1 https://github.com/Subarctic2796/blam.git
cd blam
make build
./blam
```

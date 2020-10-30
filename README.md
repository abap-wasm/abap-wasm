# abap-wasm
Somewhat free interpretation of a very basic [WebAssembly](https://webassembly.github.io/spec/core/) Virtual Machine in ABAP.

* Works on 702 and up, including [Steampunk](https://blogs.sap.com/2019/08/20/its-steampunk-now/)
* Complete Continious Integration(CI) setup

Install via [abapGit](https://abapgit.org)

## WebAssembly(WASM)
A lot of [awesome](https://github.com/mbasso/awesome-wasm) projects and programming languages can be compiled to WASM. Being able to execute WASM in ABAP would open a large new ecosystem of possibilities on the ABAP stack.

It will probably be slow to execute WASM on the ABAP application server, time will tell.

## Version Support
abap-wasm is developed using some [new ABAP syntax](https://abaplint.app/stats/larshp/abap-wasm/statement_compatibility), in order to still support 702, the source code is automatically downported using the downport functionality in [abaplint](https://abaplint.org).

Example:


## CI/Test Setup
Transpiling
unreliable, but here and now for everyone

## Future Work
WAT and WAST parser not functioning
coverage results via CI
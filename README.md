# abap-wasm
Somewhat free interpretation of a very basic [WebAssembly](https://webassembly.github.io/spec/core/) Virtual Machine in ABAP.

* Works on 702 and up, including [Steampunk](https://blogs.sap.com/2019/08/20/its-steampunk-now/) / ABAP Cloud programming model
* Complete Continuous Integration setup

Install via [abapGit](https://abapgit.org)

Usage: see [unit tests in ZCL_WASM](https://github.com/larshp/abap-wasm/blob/master/src/zcl_wasm.clas.testclasses.abap#L16)

## WebAssembly(WASM)
A lot of [awesome](https://github.com/mbasso/awesome-wasm) projects and programming languages can be compiled to WASM. Being able to execute WASM in ABAP would open a large new ecosystem of possibilities on the ABAP stack.

It will probably be slow to execute WASM on the ABAP application server, time will tell.
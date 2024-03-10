import * as fs from "fs";

import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

const hex = fs.readFileSync("./node_modules/@jitl/quickjs-wasmfile-debug-sync/dist/emscripten-module.wasm").toString("hex").toUpperCase();
const xstr = new abap.types.XString();
xstr.set(hex);

const lv_json = await abap.Classes["CL_QUICKJS"].run({iv_hex: xstr});

if (fs.existsSync("web") === false) {
  fs.mkdirSync("web");
}
fs.writeFileSync("web/quickjs.json", lv_json.get());
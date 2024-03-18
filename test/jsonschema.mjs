import * as fs from "fs";

import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

const hex = fs.readFileSync("./node_modules/@larshp/jsonschema-rs-wasm/jsonschema_rs_wasm_bg.wasm").toString("hex").toUpperCase();
const xstr = new abap.types.XString();
xstr.set(hex);

const lv_json = await abap.Classes["CL_JSONSCHEMA"].run({iv_hex: xstr});

if (fs.existsSync("web") === false) {
  fs.mkdirSync("web");
}
fs.writeFileSync("web/jsonschema.json", lv_json.get());
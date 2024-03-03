import * as fs from "fs";
import * as sha256 from "sha256-wasm";

import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

// https://www.npmjs.com/package/sha256-wasm
var hash = sha256.default().update('hello world').digest('hex');
console.log(hash);

const lv_json = await abap.Classes["CL_SHA256"].run();

if (fs.existsSync("web") === false) {
  fs.mkdirSync("web");
}
fs.writeFileSync("web/sha256.json", lv_json.get());
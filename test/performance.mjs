import * as fs from "fs";

import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

const lv_json = await abap.Classes["ZCL_WASM_PERFORMANCE"].run();

if (fs.existsSync("web") === false) {
  fs.mkdirSync("web");
}
fs.writeFileSync("web/performance.json", lv_json.get());
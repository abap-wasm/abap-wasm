import * as fs from "fs";

import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

const lv_json = await abap.Classes["CL_QUICKJS"].run();

if (fs.existsSync("web") === false) {
  fs.mkdirSync("web");
}
fs.writeFileSync("web/quickjs.json", lv_json.get());
import * as fs from "fs";

import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

const lv_json = await abap.Classes["CL_SCRYPT"].run();

if (fs.existsSync("web") === false) {
  fs.mkdirSync("web");
}
fs.writeFileSync("web/scrypt.json", lv_json.get());
import * as fs from "fs";

import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

const html = await abap.Classes["CL_TESTSUITE"].run();

if (fs.existsSync("web") === false) {
  fs.mkdirSync("web");
}
fs.writeFileSync("web/testsuite.html", html.get());
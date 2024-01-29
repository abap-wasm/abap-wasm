import * as fs from "fs";

import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

const lo_result = await abap.Classes["CL_TESTSUITE"].run();

const html = (await lo_result.get().render_html()).get();

if (fs.existsSync("web") === false) {
  fs.mkdirSync("web");
}
fs.writeFileSync("web/testsuite.html", html);
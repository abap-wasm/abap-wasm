import * as fs from "fs";

import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

await abap.Classes["CL_QUICKJS"].run();
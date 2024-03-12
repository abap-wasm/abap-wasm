import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

await abap.Classes["ZCL_WASM_PERFORMANCE"].run();
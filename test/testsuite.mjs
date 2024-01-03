import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

await abap.Classes["CL_TESTSUITE"].run();
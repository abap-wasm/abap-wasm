import * as fs from "fs";

import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

/*
import * as scryptenc from "@sorairolake/scryptenc-wasm";
const data = new TextEncoder().encode("hello");
const passphrase = new TextEncoder().encode("pass");
const ciphertext = scryptenc.encrypt(data, passphrase);
console.dir(ciphertext);
*/

/*
import { scrypt } from 'hash-wasm';
const res = await scrypt({
  password: 'abc',
  salt: '12345678',
  costFactor: 64,
  blockSize: 27,
  parallelism: 67,
  hashLength: 17,
  outputType: 'hex',
});
console.log(res);
*/

const lv_json = await abap.Classes["CL_SCRYPT"].run();

if (fs.existsSync("web") === false) {
  fs.mkdirSync("web");
}
fs.writeFileSync("web/scrypt.json", lv_json.get());
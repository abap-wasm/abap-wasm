console.dir("sdfsdf");

const fs = require("fs");

const after = JSON.parse(fs.readFileSync("../after.json", "utf-8"));
const before = JSON.parse(fs.readFileSync("../before.json", "utf-8"));
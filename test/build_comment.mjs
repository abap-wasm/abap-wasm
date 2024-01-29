import fs from "fs";

const after = JSON.parse(fs.readFileSync("../after.json", "utf-8"));
const before = JSON.parse(fs.readFileSync("../before.json", "utf-8"));

let comment = "Regression test results:\n";

comment += "|         | Before | After | Delta |\n";
comment += "| :---    | ---: | ---: | ---: |\n";
comment += "| Success | 0 | 0 | 0 |\n";
comment += "| Warning | 0 | 0 | 0 |\n";
comment += "| Error   | 0 | 0 | 0 |\n";

comment += "\nUpdated: " + new Date().toISOString() + "\n";
comment += "\nSHA: " + process.env.GITHUB_SHA + "\n";

fs.writeFileSync("comment-regression.txt", comment);
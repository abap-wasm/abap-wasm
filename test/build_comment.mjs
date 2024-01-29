import fs from "fs";

const after = JSON.parse(fs.readFileSync("../after.json", "utf-8"));
const before = JSON.parse(fs.readFileSync("../before.json", "utf-8"));

let comment = "Regression test results:\n";

comment += "|           | Before | After | Delta |\n";
comment += "| :---      | ---:   | ---:  | ---:  |\n";
comment += `| Successes | ${before.successes} | ${after.successes} | ${after.successes - before.successes} |\n`;
comment += `| Warnings  | ${before.warnings} | ${after.warnings} | ${after.warnings - before.warnings} |\n`;
comment += `| Errors    | ${before.errors} | ${after.errors} | ${after.errors - before.errors} |\n`;

comment += "\nUpdated: " + new Date().toISOString() + "\n";
comment += "\nSHA: " + process.env.GITHUB_SHA + "\n";

fs.writeFileSync("comment-regression.txt", comment);
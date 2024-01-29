import fs from "fs";

const after = JSON.parse(fs.readFileSync("../after.json", "utf-8"));
const before = JSON.parse(fs.readFileSync("../before.json", "utf-8"));

let comment = "Regression test results:\n";

comment += "|           | Before | After | Delta |\n";
comment += "| :---      | ---:   | ---:  | ---:  |\n";
comment += `| :green_circle: Successes | ${before.successes} | ${after.successes} | ${after.successes - before.successes} |\n`;
comment += `| :yellow_circle: Warnings | ${before.warnings}  | ${after.warnings}  | ${after.warnings - before.warnings} |\n`;
comment += `| :red_circle: Errors      | ${before.errors}    | ${after.errors}    | ${after.errors - before.errors} |\n`;
comment += `| :running_man: Runtime    | ${before.runtime}s  | ${after.runtime}s  | ${after.runtime - before.runtime}s |\n`;

comment += "\nUpdated: " + new Date().toISOString() + "\n";
comment += "\nSHA: " + process.env.GITHUB_SHA + "\n";

fs.writeFileSync("comment-regression.txt", comment);
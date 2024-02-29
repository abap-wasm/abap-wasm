import fs from "fs";

let folders = [];
for (const dir of fs.readdirSync("testsuite", {withFileTypes: true})) {
  if (dir.isDirectory() === false) {
    continue;
  }
  folders.push(dir.name);
}

let comment = "Regression test results:\n\n";

comment += "|           | Successes | Warnings | Errors | Runtime |\n";
comment += "| :---      | ---:      | ---:     | ---:   | ---:    |\n";

let totalSuccessesAfter = 0;
let totalSuccessesBefore = 0;
for (const folder of folders) {
  const after = JSON.parse(fs.readFileSync(`../after/${folder}.json`, "utf-8"));
  const before = JSON.parse(fs.readFileSync(`../before/${folder}.json`, "utf-8"));

  const good = " :white_check_mark:";
  const bad = " :small_red_triangle_down:"
  let successes = after.successes;
  totalSuccessesAfter += after.successes;
  totalSuccessesBefore += before.successes;
  if (before.successes !== after.successes) {
    let delta = after.successes - before.successes;
    successes += " (" + delta + (delta > 0 ? good : bad ) + ")";
  }
  let warnings = after.warnings;
  let errors = after.errors;
  let runtime = after.runtime;
  comment += `| ${folder} | ${successes}  | ${warnings}  | ${errors} | ${runtime}s |\n`;
}

comment += `Total Succeses: *${totalSuccessesAfter}*`;
if (totalSuccessesBefore !== totalSuccessesAfter) {
  let delta = totalSuccessesAfter - totalSuccessesBefore;
  comment += " (" + delta + (delta > 0 ? good : bad ) + ")";
}
comment += "\n";

comment += "\nUpdated: " + new Date().toISOString() + "\n";
comment += "\nSHA: " + process.env.GITHUB_SHA + "\n";

fs.writeFileSync("comment-regression.txt", comment);
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

const good = " :white_check_mark:";
const bad = " :small_red_triangle_down:"

let totalSuccessesAfter = 0;
let totalSuccessesBefore = 0;
let totalErrors = 0;
let totalWarnings = 0;
let totalRuntime = 0;
for (const folder of folders) {
  const after = JSON.parse(fs.readFileSync(`../after/${folder}.json`, "utf-8"));
  const before = JSON.parse(fs.readFileSync(`../before/${folder}.json`, "utf-8"));

  let successes = after.successes;
  totalSuccessesAfter += after.successes;
  totalErrors += after.errors;
  totalWarnings += after.warnings;
  totalSuccessesBefore += before.successes;
  if (before.successes !== after.successes) {
    let delta = after.successes - before.successes;
    successes += " (" + delta + (delta > 0 ? good : bad ) + ")";
  }
  let warnings = after.warnings;
  let errors = after.errors;
  let runtime = after.runtime;
  totalRuntime += runtime;
  comment += `| ${folder} ([old](https://abap-wasm.github.io/abap-wasm-web/${folder}.html)) | ${successes} | ${warnings}  | ${errors} | ${runtime}s |\n`;
}

comment += `| | **${totalSuccessesAfter}**`;
if (totalSuccessesBefore !== totalSuccessesAfter) {
  let delta = totalSuccessesAfter - totalSuccessesBefore;
  comment += " (" + delta + (delta > 0 ? good : bad ) + ")";
}
comment += ` | ${totalWarnings} | ${totalErrors} | ${totalRuntime}s |\n\n\n`;

const quickjsAfter = JSON.parse(fs.readFileSync(`../after/quickjs.json`, "utf-8"));
const quickjsBefore = JSON.parse(fs.readFileSync(`../before/quickjs.json`, "utf-8"));
const scryptAfter = JSON.parse(fs.readFileSync(`../after/scrypt.json`, "utf-8"));
const scryptBefore = JSON.parse(fs.readFileSync(`../before/scrypt.json`, "utf-8"));
const sha256After = JSON.parse(fs.readFileSync(`../after/sha256.json`, "utf-8"));
const sha256Before = JSON.parse(fs.readFileSync(`../before/sha256.json`, "utf-8"));
comment += "Performance:\n";
comment += "|           | Before | After | Delta |\n";
comment += "| :---      | ---:   | ---:  | ---:  |\n";
comment += `| :partying_face: QuickJS | ${quickjsBefore.runtime}ms | ${quickjsAfter.runtime}ms | ${quickjsAfter.runtime - quickjsBefore.runtime}ms |\n`;
comment += `| :money_mouth_face: Scrypt | ${scryptBefore.runtime}ms | ${scryptAfter.runtime}ms | ${scryptAfter.runtime - scryptBefore.runtime}ms |\n`;
comment += `| :sunglasses: SHA256 | ${sha256Before.runtime}ms | ${sha256After.runtime}ms | ${sha256After.runtime - sha256Before.runtime}ms |\n`;

const performanceAfter = JSON.parse(fs.readFileSync(`../after/performance.json`, "utf-8"));
const performanceBefore = JSON.parse(fs.readFileSync(`../before/performance.json`, "utf-8"));
for (const row of performanceAfter) {
  let before = 0;
  for (const bar of performanceBefore) {
    if (bar.DESCRIPTION === row.DESCRIPTION) {
      before = bar.TIME;
      break;
    }
  }
  comment += `| ${row.DESCRIPTION} | ${before}ms | ${row.TIME}ms | ${row.TIME - before}ms |\n`;
}

comment += "\nUpdated: " + new Date().toISOString() + "\n";
comment += "\nSHA: " + process.env.GITHUB_SHA + "\n";

fs.writeFileSync("comment-regression.txt", comment);

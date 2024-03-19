import * as fs from "fs";
import * as path from "path";

const FOLDER = "dist";

if (fs.existsSync(FOLDER)) {
  fs.rmSync(FOLDER, { recursive: true });
}
fs.mkdirSync(FOLDER);
fs.mkdirSync(FOLDER + path.sep + "src");

for (const r of fs.readdirSync("src", { withFileTypes: true, recursive: true })) {
  if (r.isFile()) {
    let text = fs.readFileSync(r.path + path.sep + r.name).toString("utf-8");
    text = text.replace(/\s*"##feature-start=debug([\w\W]*?)"##feature-end=debug/, "");
    fs.writeFileSync(FOLDER + path.sep + r.path + path.sep + r.name, text);
  } else {
    fs.mkdirSync(FOLDER + path.sep + r.path + path.sep + r.name);
  }
}

fs.copyFileSync(".abapgit.xml", FOLDER + path.sep + ".abapgit.xml");
fs.copyFileSync("LICENSE", FOLDER + path.sep + "LICENSE");
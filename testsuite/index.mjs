import * as fs from 'node:fs';
import * as child_process from 'node:child_process';

for (const filename of fs.readdirSync('.')) {
  if (filename.endsWith('.wast') === false) {
    continue;
  }

  console.log(filename);
  const name = filename.split('.')[0];
  if (fs.existsSync(name) === false) {
    fs.mkdirSync(name);
  }

  // todo, wast2sjon doesnt handle these 2 properly
  if (name === "comments" || name === "if") {
    continue;
  }

  child_process.execSync(`rm -f ${name}/*.*`);
  child_process.execSync(`wast2json ${filename} -o ${name}/${name}.json`);
}
import * as fs from 'node:fs';

for (const filename of fs.readdirSync('.')) {
  if (filename.endsWith('.wast') === false) {
    continue;
  }

  console.log(filename);
  const name = filename.split('.')[0];
  if (fs.existsSync(name) === false) {
    fs.mkdirSync(name);
  }

}
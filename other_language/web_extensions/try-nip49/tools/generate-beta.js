import fs from 'node:fs/promises';

console.log("generate beta");

const [ ncsfp, pssfp, npbfp ]  = process.argv.slice(2);

console.log(ncsfp, pssfp, npbfp);

const ncryptsec = (new TextDecoder().decode(await fs.readFile(ncsfp))).trimEnd();
const password = (new TextDecoder().decode(await fs.readFile(pssfp))).trimEnd();
const npub = (new TextDecoder().decode(await fs.readFile(npbfp))).trimEnd();

const result = `export const ncryptsec = "${ncryptsec}";
export const password = "${password}";
export const npub = "${npub}";
`

console.log(result);

await fs.writeFile("generated/sampleKeyPair.js", result);

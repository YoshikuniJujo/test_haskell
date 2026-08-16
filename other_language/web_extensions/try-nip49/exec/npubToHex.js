import fs from 'node:fs/promises';
import * as Bech32 from '../src/bech32.js'

const npub = new TextDecoder().decode(await fs.readFile("/home/tatsuya/tmp/npub"));

console.log(Bech32.decodeNpubToHex(npub));

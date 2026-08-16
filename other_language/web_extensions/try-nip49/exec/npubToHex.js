import fs from 'node:fs/promises';
import * as Bech32 from '../src/bech32.js'

const npub = new TextDecoder().decode(await fs.readFile("/home/tatsuya/tmp/npub"));

function decodeNpubToHex(np) {
	const decoded = Bech32.decode(npub);
	return Array.from(decoded, b => b.toString(16).padStart(2, "0")).join("");
}

console.log(Bech32.decodeNpubToHex(npub));

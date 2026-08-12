import fs from 'node:fs/promises';
import { generate, verify, word30ToWord5List } from '../src/polymod.js';

function hrpExpand(hrp) {
	const bs = hrp.map(c => c.charCodeAt(0));
	return [
		...bs.map(b => b >>> 5),
		0,
		...bs.map(b => b & 0x1f)
	];
}

const charset = 'qpzry9x8gf2tvdw0s3jn54khce6mua7l';

function dpToWord5s(dp) {
	return dp.map(c => charset.indexOf(c));
}

const cs = generate([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
const ws2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, ...word30ToWord5List(cs)];

console.log(generate([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
console.log((word30ToWord5List(1061443723)));
console.log(cs)
console.log(ws2)
console.log(verify(ws2));

const buffer = await fs.readFile('../../../themes/nostr/nip49/try-nip49/test_vectors/test00.ncryptsec');
const text = new TextDecoder().decode(buffer);
const chars = [...text];
console.log(chars);

const i = chars.lastIndexOf('1');
const hrp = chars.slice(0, i);
const dp = chars.slice(i + 1);

console.log(hrp);
console.log(dp);

const hrp5 = hrpExpand(hrp);
const dp5 = dpToWord5s(dp);

console.log(hrp5);
console.log(dp5);

console.log(verify([...hrp5, ...dp5]))

const decoded = {
	humanReadablePart: hrp.join(''),
	dataPart: dp5.slice(0, -6) }

console.log(decoded);

function split(bs, ns) {
	if (ns.length === 0) {
		return [];
	}

	const [n, ...rest] = ns;
	return [bs.slice(0, n), ...split(bs.slice(n), rest)];
}

const [vsn, lgn, slt, nnc, aad, ct, mac] = split(decoded.dataPart, [1, 1, 16, 24, 1, 32, 16]);

console.log(vsn, lgn, slt, nnc, aad, ct, mac);

const symKeyPrms = { logN: lgn[0], salt: slt }


const encrypted = {
	version: vsn[0], nonce: nnc, keySecurityByte: aad[0],
	cipherText: ct, mac: mac }

console.log(symKeyPrms, encrypted);

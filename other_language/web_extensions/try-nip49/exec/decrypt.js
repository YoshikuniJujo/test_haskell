import fs from 'node:fs/promises';
import { generate, verify, word30ToWord5List } from '../src/polymod.js';
import { scrypt } from '@noble/hashes/scrypt.js';

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
const password = await fs.readFile('../../../themes/nostr/nip49/try-nip49/test_vectors/test00.password');
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

function chunks(n, xs) {
	const ln = xs.length;
	if (ln < n) {
		return { init: [], last: xs, lastN: ln };
	}
	const ys = chunks(n, xs.slice(n));
	const init = ys.init;
	const last = ys.last;
	const lastN = ys.lastN;
	return { init: [xs.slice(0, n), ...init], last, lastN };
}

const c8 = chunks(8, decoded.dataPart);

console.log(c8);
console.log(chunks(8, []));
console.log(chunks(8, [0]));
console.log(chunks(8, [0, 1]));
console.log(chunks(8, [0, 1, 2, 3, 4, 5, 6]));
console.log(chunks(8, [0, 1, 2, 3, 4, 5, 6, 7]));
console.log(chunks(8, [0, 1, 2, 3, 4, 5, 6, 7, 8]));

function word5sToWord40(ws) {
	return ws.reduce(
		(w, x) => (w << 5n) | BigInt(x),
		0n
	);
}

console.log(c8.init.map(word5sToWord40));
console.log(word5sToWord40(c8.last) << 5n * (8n - BigInt(c8.lastN)));

const c40 = {
	init: c8.init.map(word5sToWord40),
	last: word5sToWord40(c8.last) << 5n * (8n - BigInt(c8.lastN)),
	lastN: c8.lastN * 5
}

console.log(c40);

function word40ToWord8List(w) {
	return [
		Number((w >> 32n) & 0xffn),
		Number((w >> 24n) & 0xffn),
		Number((w >> 16n) & 0xffn),
		Number((w >> 8n) & 0xffn),
		Number(w & 0xffn)
	];
}

const dataPartInit = c40.init.map(word40ToWord8List);
const dataPartLast = word40ToWord8ListTail(c40.last, c40.lastN / 8);

console.log(dataPartInit);
console.log(dataPartLast);

function word40ToWord8ListTail(w, n) {
	return word40ToWord8List(w).slice(0, n);
}

const dataPart = new Uint8Array(dataPartInit.flat().concat(dataPartLast));

console.log(dataPartInit.concat(dataPartLast));
console.log(dataPart);

const [vsn, lgn, slt, nnc, aad, ct, mac] = split(dataPart, [1, 1, 16, 24, 1, 32, 16]);

console.log(vsn, lgn, slt, nnc, aad, ct, mac);

const symKeyPrms = { logN: lgn[0], salt: slt };
const encrypted = {
	version: vsn[0],
	nonce: nnc,
	keySecurityByte: aad[0],
	cipherText: ct,
	mac: mac };

console.log(symKeyPrms);
console.log(encrypted);
console.log(password);

const key = scrypt(password, symKeyPrms.salt, {
	N: 2 ** symKeyPrms.logN,
	r: 8, p: 1, dkLen: 32 });

console.log(key);

/*
const [vsn, lgn, slt, nnc, aad, ct, mac] = split(decoded.dataPart, [1, 1, 16, 24, 1, 32, 16]);

console.log(vsn, lgn, slt, nnc, aad, ct, mac);

const symKeyPrms = { logN: lgn[0], salt: slt }


const encrypted = {
	version: vsn[0], nonce: nnc, keySecurityByte: aad[0],
	cipherText: ct, mac: mac }

console.log(symKeyPrms, encrypted);
*/

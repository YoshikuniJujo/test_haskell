import fs from 'node:fs/promises';
import { scrypt } from '@noble/hashes/scrypt.js';
import { xchacha20poly1305 } from '@noble/ciphers/chacha.js';
import * as Bech32 from '../src/bech32.js';

process.stdout.on('error', err => {
	if (err.code === 'EPIPE') { process.exit(0); }
	throw err; });

const buffer = await fs.readFile(
	'../../../themes/nostr/nip49/try-nip49/test_vectors/test00.ncryptsec' );
const password = await fs.readFile(
	'../../../themes/nostr/nip49/try-nip49/test_vectors/test00.password' );
const text = new TextDecoder().decode(buffer);
const password2 = new TextDecoder().decode(password);

const decoded = Bech32.decode(text);

console.log('DECODED: ', decoded);

const [vsn, lgn, slt, nnc, aad, ct, mac] =
	split(decoded, [1, 1, 16, 24, 1, 32, 16]);

function split(bs, ns) {
	if (ns.length === 0) { return []; }
	const [n, ...rest] = ns;
	return [bs.slice(0, n), ...split(bs.slice(n), rest)]; }

console.log(vsn, lgn, slt, nnc, aad, ct, mac);

console.log("NONCE: ", nnc);

const symKeyPrms = { logN: lgn[0], salt: slt };
const encrypted = {
	version: vsn[0], nonce: nnc,
	keySecurityByte: aad[0], cipherText: ct, mac: mac };

console.log(symKeyPrms);
console.log(encrypted);
console.log(password);

const smkey = scrypt(password2.normalize("NFKC"), symKeyPrms.salt,
	{ N: 2 ** symKeyPrms.logN, r: 8, p: 1, dkLen: 32 });

console.log(smkey);

const ciphertext = new Uint8Array([...ct, ...mac]);
const chacha = xchacha20poly1305(smkey, encrypted.nonce, aad);
const secretKey = chacha.decrypt(ciphertext);

console.log(secretKey);
console.log(Bech32.encode('nsec', secretKey));

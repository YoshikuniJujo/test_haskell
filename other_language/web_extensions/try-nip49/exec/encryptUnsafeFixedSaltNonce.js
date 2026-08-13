import fs from 'node:fs/promises';
import { scrypt } from '@noble/hashes/scrypt.js';
import { xchacha20poly1305 } from '@noble/ciphers/chacha.js'
import * as Bech32 from '../src/bech32.js';

console.log("UNSAFE FIXED SALT AND NONCE ENCRYPTER");

const buffer = await fs.readFile('../../../themes/nostr/nip49/try-nip49/test_vectors/test00.nsec');
const password = await fs.readFile('../../../themes/nostr/nip49/try-nip49/test_vectors/test00.password');
const text = new TextDecoder().decode(buffer);
const seckey = Bech32.decode(text);

console.log(text);
console.log(Bech32.decode(text));

const salt = new Uint8Array([
	55, 255, 75, 198, 238, 145, 63, 237,
	106, 172, 118, 243, 250, 254, 78, 104 ]);

const nonce = new Uint8Array([
	129, 12, 236, 49, 110, 48, 213, 101, 27, 172, 252, 96, 193, 29,
	226, 116, 94, 19, 113, 28, 114, 246, 0, 162 ]);

const smkey = scrypt(password, salt, { N: 2 ** 16, r: 8, p: 1, dkLen: 32 });

console.log(salt);
console.log(smkey);

console.log(nonce);

const chacha = xchacha20poly1305(smkey, nonce, new Uint8Array([0]));
const encrypted = chacha.encrypt(seckey);

console.log(Bech32.encode('ncryptsec', new Uint8Array([2, 16, ...salt, ...nonce, 0, ...encrypted])));

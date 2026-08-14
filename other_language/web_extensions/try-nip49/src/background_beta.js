import fs from 'node:fs/promises';
import { scrypt } from '@noble/hashes/scrypt.js';
import { xchacha20poly1305 } from '@noble/ciphers/chacha.js';
import * as Bech32 from './bech32.js';
import * as Nip49 from './nip49.js';
import { ncryptsec, password, npub } from '../generated/sampleKeyPair.js';

const ncryptsec_unbech32 = Bech32.decode(ncryptsec);

console.log(ncryptsec_unbech32);

const [vsn, lgn, slt, nnc, aad, ct, mac] =
	Nip49.split(ncryptsec_unbech32, [1, 1, 16, 24, 1, 32, 16]);

const symKeyPrms = { logN: lgn[0], salt: slt };
const encrypted = {
	version: vsn[0], nonce: nnc,
	keySecurityByte: aad[0], cipherText: ct, mac: mac };

console.log(symKeyPrms);
console.log(encrypted);

const smkey = scrypt(password, symKeyPrms.salt,
	{ N: 2 ** symKeyPrms.logN, r: 8, p: 1, dkLen: 32 });

console.log(smkey);

const ciphertext = new Uint8Array([...ct, ...mac]);
const chacha = xchacha20poly1305(smkey, encrypted.nonce, aad);
const secretKey = chacha.decrypt(ciphertext);

console.log(Bech32.encode('nsec', secretKey));

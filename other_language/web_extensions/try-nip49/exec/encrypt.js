import fs from 'node:fs/promises';
import { webcrypto } from 'node:crypto';
import { scrypt } from '@noble/hashes/scrypt.js';
import { xchacha20poly1305 } from '@noble/ciphers/chacha.js'
import * as Bech32 from '../src/bech32.js';

const buffer = await fs.readFile('../../../themes/nostr/nip49/try-nip49/test_vectors/test00.nsec');
const text = new TextDecoder().decode(buffer);
const seckey = Bech32.decode(text);
const password = await fs.readFile('../../../themes/nostr/nip49/try-nip49/test_vectors/test00.password');
const password2 = new TextDecoder().decode(password);

console.log(text);
console.log(Bech32.decode(text));

const salt = new Uint8Array(16);
const nonce = new Uint8Array(24);

webcrypto.getRandomValues(salt);
webcrypto.getRandomValues(nonce);

const smkey = scrypt(password2.normalize("NFKC"), salt, { N: 2 ** 16, r: 8, p: 1, dkLen: 32 });

console.log(salt);
console.log(smkey);

console.log(nonce);

const chacha = xchacha20poly1305(smkey, nonce, new Uint8Array([0]));
const encrypted = chacha.encrypt(seckey);

const ncryptsec = Bech32.encode('ncryptsec', new Uint8Array([2, 16, ...salt, ...nonce, 0, ...encrypted]));
console.log(ncryptsec);

fs.writeFile('test_vectors/test00.ncryptsec', ncryptsec);
fs.writeFile('test_vectors/test00.password', password);

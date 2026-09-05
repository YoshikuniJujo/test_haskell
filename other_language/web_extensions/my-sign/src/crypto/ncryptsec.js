import { webcrypto } from 'node:crypto';
import { scrypt } from '@noble/hashes/scrypt.js';
import { xchacha20poly1305 } from '@noble/ciphers/chacha.js'

export async function
encrypt(secKey, { password: pswd, logN: lgn, keySecurityByte: ksb })
{

	const salt = new Uint8Array(16);
	const nonce = new Uint8Array(24);

	webcrypto.getRandomValues(salt);
	webcrypto.getRandomValues(nonce);

	const smkey = scrypt(
		new TextEncoder().encode(pswd.normalize("NFKC")),
		salt, { N: 2 ** lgn, r: 8, p: 1, dkLen: 32 } );

	const chacha = xchacha20poly1305(smkey, nonce, new Uint8Array([ksb]));

	return {
		version: 2,
		logN: lgn,
		salt: salt,
		nonce: nonce,
		keySecurityByte: ksb,
		ciphertext: chacha.encrypt(secKey) }

}

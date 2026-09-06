import { webcrypto } from 'node:crypto';
import { scrypt } from '@noble/hashes/scrypt.js';
import { xchacha20poly1305 } from '@noble/ciphers/chacha.js'
import * as Bech32 from "../codec/bech32.js";

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

export function
encode(foo)
{

	return Bech32.encode('ncryptsec',
		new Uint8Array([
			foo.version, foo.logN, ...foo.salt, ...foo.nonce,
			foo.keySecurityByte, ...foo.ciphertext ]));
}

export async function
decrypt(encrypted, pswd)
{

	if (encrypted.version !== 2) throw new Error(
		`Invalid ncryptsec version: expected 2, actual ${encrypted.version}` );
	if (encrypted.keySecurityByte > 2) throw new Error(
		`Invalid key security byte: expected 0, 1, or 2, actual ${encrypted.keySecurityByte}` );
	if (encrypted.logN < 16 || 22 < encrypted.logN) throw new Error(
		`Unsupported scrypt log_n: expected 16..22, actual ${encrypted.logN}` );

	const smkey = scrypt(new TextEncoder().encode(pswd.normalize("NFKC")), encrypted.salt,
		{ N: 2 ** encrypted.logN, r: 8, p: 1, dkLen: 32 });

	const chacha = xchacha20poly1305(smkey,
		encrypted.nonce,
		new Uint8Array([encrypted.keySecurityByte]));
	return chacha.decrypt(encrypted.ciphertext);
}

export function
decode(text)
{

const text2 = text.trim();

const { dp: decoded } = Bech32.decode(text2);

if (decoded.length !== 91) throw new Error(
	`Invalid ncryptsec length: expected 91, actual ${decoded.length}` );

const [vsn, lgn, slt, nnc, aad, ct] =
	split(decoded, [1, 1, 16, 24, 1, 48]);

const encrypted = {
	version: vsn[0], logN: lgn[0], salt: slt, nonce: nnc,
	keySecurityByte: aad[0], ciphertext: ct };

return encrypted;

}

function
split(bs, ns)
{
	if (ns.length === 0) { return []; }
	const [n, ...rest] = ns;
	return [bs.slice(0, n), ...split(bs.slice(n), rest)]; }

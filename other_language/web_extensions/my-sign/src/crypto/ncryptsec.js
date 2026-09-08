import { webcrypto } from 'node:crypto';
import { scryptAsync } from '@noble/hashes/scrypt.js';
import { xchacha20poly1305 } from '@noble/ciphers/chacha.js'
import { schnorr } from "@noble/secp256k1";
import * as Bech32 from "../codec/bech32.js";

import * as Schnorr from "../sign/schnorr.js"

export class
EncryptedSecretKey
{
	#publicKey;

	#logN;
	#salt;
	#nonce;
	#keySecurityByte;
	#ciphertext;

	#saltForCheckPassword;
	#hashForCheckPassword;

	constructor(pk, ln, slt, nnc, ksb, ct, sfcp, hfcp)
	{
		if (ksb > 2) throw new Error(
			`Invalid key security byte: expected 0, 1, or 2, actual ${ksb}` );
		if (ln < 16 || 22 < ln) throw new Error(
			`Unsupported scrypt log_n: expected 16..22, actual ${ln}` );

		this.#publicKey = pk;
		this.#logN = ln;
		this.#salt = slt;
		this.#nonce = nnc;
		this.#keySecurityByte = ksb;
		this.#ciphertext = ct;
		this.#saltForCheckPassword = sfcp;
		this.#hashForCheckPassword = hfcp;
	}

	static async generate(pswd)
	{
		const lgn = 16
		const sfcp = new Uint8Array(16);
		webcrypto.getRandomValues(sfcp);
		const hfcp = scryptAsync(
			new TextEncoder().encode(pswd.normalize("NFKC")),
			sfcp, { N: 2 ** lgn, r: 8, p: 1, dkLen: 32 } );
		const { secretKey: sk, publicKey: pk } = schnorr.keygen();
		let foo;
		try { foo = await encrypt(
				sk, { password: pswd, logN: lgn, keySecurityByte: 1 } ); }
		finally { sk.fill(0); }
		return new EncryptedSecretKey(
			pk, foo.logN, foo.salt, foo.nonce,
			foo.keySecurityByte, foo.ciphertext, sfcp, await hfcp );

	}

	static fromEncrypted(encrypted, pswd)
	{
		if (encrypted.version !== 2) throw new Error(
			`Invalid ncryptsec version: expected 2, actual ${encrypted.version}` );
		return this.#fromEncrypted(
			encrypted.logN,
			encrypted.salt,
			encrypted.nonce,
			encrypted.keySecurityByte,
			encrypted.ciphertext,
			pswd );
	}

	static async #fromEncrypted(lgn, slt, nnc, ksb, ct, pswd)
	{
		if (ksb > 2) throw new Error(
			"Invalid key security byte: expected 0, 1, or 2, " +
			"actual " + ksb );
		if (lgn < 16 || 22 < lgn) throw new Error(
			"Unsupported scrypt log_n: expected 16..22, " +
			"actual " + lgn );

		const smkey = scryptAsync(
			new TextEncoder().encode(pswd.normalize("NFKC")), slt,
			{ N: 2 ** lgn, r: 8, p: 1, dkLen: 32 } );

		const cc = xchacha20poly1305(await smkey, nnc, new Uint8Array([ksb]));
		const sk = cc.decrypt(ct);
		let pk;
		try { pk = schnorr.getPublicKey(sk); }
		finally { sk.fill(0); }

		const sfcp = new Uint8Array(16);
		webcrypto.getRandomValues(sfcp);
		const hfcp = await scryptAsync(
			new TextEncoder().encode(pswd.normalize("NFKC")),
			sfcp, { N: 2 ** lgn, r: 8, p: 1, dkLen: 32 } );
		return new EncryptedSecretKey(
			pk, lgn, slt, nnc, ksb, ct, sfcp, hfcp );
	}

	get publicKey()
	{
		return this.#publicKey
	}

	toObject_563e7e39d4()
	{
		return {
			publicKey: this.#publicKey,
			version: 2,
			logN: this.#logN,
			salt: this.#salt,
			nonce: this.#nonce,
			keySecurityByte: this.#keySecurityByte,
			ciphertext: this.#ciphertext,
			saltForCheckPassword: this.#saltForCheckPassword,
			hashForCheckPassword: this.#hashForCheckPassword }
	}

	async checkPassword(pswd)
	{
		const hfcp = await scryptAsync(
			new TextEncoder().encode(pswd.normalize("NFKC")),
			this.#saltForCheckPassword,
			{ N: 2 ** this.#logN, r: 8, p: 1, dkLen: 32 } );
		return equalBytes(hfcp, this.#hashForCheckPassword);
	}

	async getSymmetricKey(pswd)
	{
		return scryptAsync(
			new TextEncoder().encode(pswd.normalize("NFKC")),
			this.#salt,
			{ N: 2 ** this.#logN, r: 8, p: 1, dkLen: 32 } );
	}

	async signEvent(ev, smky)
	{
		const cc = xchacha20poly1305(smky, this.#nonce, new Uint8Array([this.#keySecurityByte]));
		const sk = cc.decrypt(this.#ciphertext);
		try {
			return await Schnorr.signEvent(ev, sk, this.#publicKey);
		}
		finally { sk.fill(0); }
	}
}

export function
encode(foo)
{

	return Bech32.encode('ncryptsec',
		new Uint8Array([
			foo.version, foo.logN, ...foo.salt, ...foo.nonce,
			foo.keySecurityByte, ...foo.ciphertext ]));
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

async function
encrypt(secKey, { password: pswd, logN: lgn, keySecurityByte: ksb })
{

	const salt = new Uint8Array(16);
	const nonce = new Uint8Array(24);

	webcrypto.getRandomValues(salt);
	webcrypto.getRandomValues(nonce);

	const smkey = await scryptAsync(
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

function
split(bs, ns)
{
	if (ns.length === 0) { return []; }
	const [n, ...rest] = ns;
	return [bs.slice(0, n), ...split(bs.slice(n), rest)]; }

function
equalBytes(a, b)
{
	if (a.length !== b.length) return false;

	let d = 0;
	for (let i = 0; i < a.length; ++ i)
		d |= a[i] ^ b[i];

	return d === 0;
}

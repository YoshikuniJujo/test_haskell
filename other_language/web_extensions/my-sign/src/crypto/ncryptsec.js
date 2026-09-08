import { webcrypto } from 'node:crypto';
import { scrypt } from '@noble/hashes/scrypt.js';
import { xchacha20poly1305 } from '@noble/ciphers/chacha.js'
import { schnorr } from "@noble/secp256k1";
import * as Bech32 from "../codec/bech32.js";

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
		const hfcp = scrypt(
			new TextEncoder().encode(pswd.normalize("NFKC")),
			sfcp, { N: 2 ** lgn, r: 8, p: 1, dkLen: 32 } );
		const { secretKey: sk, publicKey: pk } = schnorr.keygen();
		let foo;
		try { foo = await encrypt(
				sk, { password: pswd, logN: lgn, keySecurityByte: 1 } ); }
		finally { sk.fill(0); }
		return new EncryptedSecretKey(
			pk, foo.logN, foo.salt, foo.nonce,
			foo.keySecurityByte, foo.ciphertext, sfcp, hfcp );

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

	static #fromEncrypted(lgn, slt, nnc, ksb, ct, pswd)
	{
		if (ksb > 2) throw new Error(
			"Invalid key security byte: expected 0, 1, or 2, " +
			"actual " + encrypted.keySecurityByte );
		if (lgn < 16 || 22 < lgn) throw new Error(
			"Unsupported scrypt log_n: expected 16..22, " +
			"actual " + encrypted.logN );

		console.log("#fromEncrypted");
		console.log(lgn);
		console.log(slt);
		console.log(nnc);
		console.log(ksb);
		console.log(ct);
		console.log(pswd);

		const smkey = scrypt(
			new TextEncoder().encode(pswd.normalize("NFKC")), slt,
			{ N: 2 ** lgn, r: 8, p: 1, dkLen: 32 } );

		const cc = xchacha20poly1305(smkey, nnc, new Uint8Array([ksb]));
		const sk = cc.decrypt(ct);
		const pk = schnorr.getPublicKey(sk);
		sk.fill(0);
		console.log(Bech32.encode("npub", pk));
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
}

async function
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

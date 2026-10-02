import { scryptAsync } from '@noble/hashes/scrypt.js';
import { randomBytes } from "@noble/hashes/utils.js";
import { xchacha20poly1305 } from '@noble/ciphers/chacha.js'
import { schnorr } from "@noble/secp256k1";
import * as Bech32 from "../codec/bech32.js";

import * as Schnorr from "../sign/schnorr.js"

type Encrypted = {
	version: number,
	logN: number,
	salt: Uint8Array,
	nonce: Uint8Array,
	keySecurityByte: number,
	ciphertext: Uint8Array
}

type Event = {
	created_at: number,
	kind: number,
	tags: string[][],
	content: string
}

export class
EncryptedSecretKey
{
	#publicKey;

	#logN;
	#name;
	#salt;
	#nonce;
	#keySecurityByte;
	#ciphertext;

	#saltForCheckPassword;
	#hashForCheckPassword;

	constructor(
		pk: Uint8Array, name: string,
		ln: number, slt: Uint8Array, nnc: Uint8Array,
		ksb: number, ct: Uint8Array, sfcp: Uint8Array, hfcp: Uint8Array)
	{
		if (ksb > 2) throw new Error(
			`Invalid key security byte: expected 0, 1, or 2, actual ${ksb}` );
		if (ln < 16 || 22 < ln) throw new Error(
			`Unsupported scrypt log_n: expected 16..22, actual ${ln}` );

		this.#publicKey = pk;
		this.#name = name;
		this.#logN = ln;
		this.#salt = slt;
		this.#nonce = nnc;
		this.#keySecurityByte = ksb;
		this.#ciphertext = ct;
		this.#saltForCheckPassword = sfcp;
		this.#hashForCheckPassword = hfcp;
	}

	static async generate(nm: string, pswd: Uint8Array)
	{
		const lgn = 16
		const sfcp = randomBytes(16);
//		const sfcp = new Uint8Array(16);
//		webcrypto.getRandomValues(sfcp);
		const hfcp = scryptAsync(
			pswd, sfcp, { N: 2 ** lgn, r: 8, p: 1, dkLen: 32 } );
		const { secretKey: sk, publicKey: pk } = schnorr.keygen();
		let foo;
		try { foo = await encrypt(
				sk, { password: pswd, logN: lgn, keySecurityByte: 1 } ); }
		finally { sk.fill(0); }
		return new EncryptedSecretKey(
			pk, nm, foo.logN, foo.salt, foo.nonce,
			foo.keySecurityByte, foo.ciphertext, sfcp, await hfcp );

	}

	static fromEncrypted(nm: string, encrypted: Encrypted, pswd: Uint8Array)
	{
		if (encrypted.version !== 2) throw new Error(
			`Invalid ncryptsec version: expected 2, actual ${encrypted.version}` );
		return this.#fromEncrypted(
			nm,
			encrypted.logN,
			encrypted.salt,
			encrypted.nonce,
			encrypted.keySecurityByte,
			encrypted.ciphertext,
			pswd );
	}

	static async #fromEncrypted(
		nm: string, lgn: number,
		slt: Uint8Array, nnc: Uint8Array, ksb: number,
		ct: Uint8Array, pswd: Uint8Array )
	{
		if (ksb > 2) throw new Error(
			"Invalid key security byte: expected 0, 1, or 2, " +
			"actual " + ksb );
		if (lgn < 16 || 22 < lgn) throw new Error(
			"Unsupported scrypt log_n: expected 16..22, " +
			"actual " + lgn );

		const smkey = scryptAsync(
			pswd, slt, { N: 2 ** lgn, r: 8, p: 1, dkLen: 32 } );

		const cc = xchacha20poly1305(await smkey, nnc, new Uint8Array([ksb]));
		const sk = cc.decrypt(ct);
		let pk;
		try { pk = schnorr.getPublicKey(sk); }
		finally { sk.fill(0); }

		const sfcp = randomBytes(16);
//		const sfcp = new Uint8Array(16);
//		webcrypto.getRandomValues(sfcp);
		const hfcp = await scryptAsync(
			pswd, sfcp, { N: 2 ** lgn, r: 8, p: 1, dkLen: 32 } );
		return new EncryptedSecretKey(
			pk, nm, lgn, slt, nnc, ksb, ct, sfcp, hfcp );
	}

	get publicKey() { return this.#publicKey }
	get name() { return this.#name }

	toObject_563e7e39d4()
	{
		return {
			publicKey: this.#publicKey,
			name: this.#name,
			version: 2,
			logN: this.#logN,
			salt: this.#salt,
			nonce: this.#nonce,
			keySecurityByte: this.#keySecurityByte,
			ciphertext: this.#ciphertext,
			saltForCheckPassword: this.#saltForCheckPassword,
			hashForCheckPassword: this.#hashForCheckPassword }
	}

	async checkPassword(pswd: Uint8Array)
	{
		const hfcp = await scryptAsync(
			pswd, this.#saltForCheckPassword,
			{ N: 2 ** this.#logN, r: 8, p: 1, dkLen: 32 } );
		return equalBytes(hfcp, this.#hashForCheckPassword);
	}

	async getSymmetricKey(pswd: Uint8Array)
	{
		return scryptAsync(
			pswd, this.#salt,
			{ N: 2 ** this.#logN, r: 8, p: 1, dkLen: 32 } );
	}

	async signEvent(ev: Event, smky: Uint8Array)
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
encode(foo: Encrypted)
{

	return Bech32.encode('ncryptsec',
		new Uint8Array([
			foo.version, foo.logN, ...foo.salt, ...foo.nonce,
			foo.keySecurityByte, ...foo.ciphertext ]));
}

export function
decode(text: string)
{

const text2 = text.trim();

const { dp: decoded } = Bech32.decode(text2);

if (decoded.length !== 91) throw new Error(
	`Invalid ncryptsec length: expected 91, actual ${decoded.length}` );

const [vsn, lgn, slt, nnc, aad, ct] =
	split(decoded, [1, 1, 16, 24, 1, 48]);

if (!vsn || !lgn || !aad) throw new Error("bad");

const encrypted = {
	version: vsn[0], logN: lgn[0], salt: slt, nonce: nnc,
	keySecurityByte: aad[0], ciphertext: ct };

return encrypted;

}

type EncryptArguments = {
	password: Uint8Array, logN: number, keySecurityByte: number
}

async function
encrypt(secKey: Uint8Array,
	{ password: pswd, logN: lgn, keySecurityByte: ksb }: EncryptArguments)
{

	const salt = randomBytes(16);
	const nonce = randomBytes(24);

	/*
	const salt = new Uint8Array(16);
	const nonce = new Uint8Array(24);

	webcrypto.getRandomValues(salt);
	webcrypto.getRandomValues(nonce);
	*/

	const smkey = await scryptAsync(
		pswd, salt, { N: 2 ** lgn, r: 8, p: 1, dkLen: 32 } );

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
split(bs: Uint8Array, ns: number[]): Uint8Array[]
{
	if (ns.length === 0) { return []; }
	const [n, ...rest] = ns;
	return [bs.slice(0, n), ...split(bs.slice(n), rest)]; }

function
equalBytes(a: Uint8Array, b: Uint8Array)
{
	if (a.length !== b.length) return false;

	let d = 0;
	for (let i = 0; i < a.length; ++ i) {
		const ai = a[i];
		const bi = b[i];
		if (ai == undefined || bi == undefined) throw new Error("bad");
		d |= ai ^ bi;
	}

	return d === 0;
}

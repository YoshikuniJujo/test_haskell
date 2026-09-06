import { readFile } from "node:fs/promises";
import { scrypt } from "@noble/hashes/scrypt.js";
import { xchacha20poly1305 } from "@noble/ciphers/chacha.js";
import * as Bech32 from "../src/codec/bech32.js";
import { readPassword } from "./readPassword.js";

const filePath = process.argv[2];

const text = (await readFile(filePath, "utf8")).trim();

const { dp: decoded } = Bech32.decode(text);

if (decoded.length !== 91) throw new Error(
	`Invalid ncryptsec length: expected 91, actual ${decoded.length}` );

function
split(bs, ns)
{
	if (ns.length === 0) { return []; }
	const [n, ...rest] = ns;
	return [bs.slice(0, n), ...split(bs.slice(n), rest)]; }

const [vsn, lgn, slt, nnc, aad, ct] =
	split(decoded, [1, 1, 16, 24, 1, 48]);

const encrypted = {
	version: vsn[0], logN: lgn[0], salt: slt, nonce: nnc,
	keySecurityByte: aad[0], ciphertext: ct };

const pswd = await readPassword();

const secretKey = await decrypt(encrypted, pswd);

console.log(Bech32.encode('nsec', secretKey));

async function
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

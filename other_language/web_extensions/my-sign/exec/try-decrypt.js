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

console.log(decoded);

function
split(bs, ns)
{
	if (ns.length === 0) { return []; }
	const [n, ...rest] = ns;
	return [bs.slice(0, n), ...split(bs.slice(n), rest)]; }

const [vsn, lgn, slt, nnc, aad, ct] =
	split(decoded, [1, 1, 16, 24, 1, 48]);

console.log(vsn);
console.log(lgn);
console.log(slt);
console.log(nnc);
console.log(aad);
console.log(ct);

if (vsn[0] !== 2) throw new Error(
	`Invalid ncryptsec version: expected 2, actual ${vsn[0]}` );
if (aad[0] > 2) throw new Error(
	`Invalid key security byte: expected 0, 1, or 2, actual ${aad[0]}` );
if (lgn[0] < 16 || 22 < lgn[0]) throw new Error(
	`Unsupported scrypt log_n: expected 16..22, actual ${lgn[0]}` );

const encrypted = {
	version: vsn[0], logN: lgn[0], salt: slt, nonce: nnc,
	keySecurityByte: aad[0], ciphertext: ct };

// const pswd = new TextEncoder().encode(await readPassword());
const pswd = await readPassword();

console.log(encrypted);
console.log(pswd);

const smkey = scrypt(pswd.normalize("NFKC"), encrypted.salt,
	{ N: 2 ** encrypted.logN, r: 8, p: 1, dkLen: 32 });

console.log(smkey);

const ciphertext = new Uint8Array(ct);
const chacha = xchacha20poly1305(smkey, encrypted.nonce, aad);
const secretKey = chacha.decrypt(ciphertext);

console.log(Bech32.encode('nsec', secretKey));

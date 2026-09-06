import { readFile } from "node:fs/promises";
import { scrypt } from "@noble/hashes/scrypt.js";
import { xchacha20poly1305 } from "@noble/ciphers/chacha.js";
import * as Bech32 from "../src/codec/bech32.js";
import { readPassword } from "./readPassword.js";

import * as Ncryptsec from "../src/crypto/ncryptsec.js"

const filePath = process.argv[2];

const text = await readFile(filePath, "utf8");

const encrypted = decode(text);

const pswd = await readPassword();

const secretKey = await Ncryptsec.decrypt(encrypted, pswd);

console.log(Bech32.encode('nsec', secretKey));

function
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

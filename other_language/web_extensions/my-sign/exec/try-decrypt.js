import { readFile } from "node:fs/promises";
import * as Bech32 from "../src/codec/bech32.js";
import { readPassword } from "./readPassword.js";

const filePath = process.argv[2];

const text = (await readFile(filePath, "utf8")).trim();

const { dp: decoded } = Bech32.decode(text);

console.log(decoded);

function
split(bs, ns)
{
	if (ns.length === 0) { return []; }
	const [n, ...rest] = ns;
	return [bs.slice(0, n), ...split(bs.slice(n), rest)]; }

const [vsn, lgn, slt, nnc, aad, ct, mac] =
	split(decoded, [1, 1, 16, 24, 1, 32, 16]);

console.log(vsn);
console.log(lgn);
console.log(slt);
console.log(nnc);
console.log(aad);
console.log(ct);
console.log(mac);

const encrypted = {
	version: vsn[0], nonce: nnc,
	keySecurityByte: aad[0], cipherText: ct, mac: mac };

const pswd = new TextEncoder().encode(await readPassword());

console.log(encrypted);
console.log(pswd);

const symKeyPrms = { logN: lgn[0], salt: slt };
console.log(symKeyPrms);

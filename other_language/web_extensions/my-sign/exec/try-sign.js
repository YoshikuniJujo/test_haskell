import { readFile } from "node:fs/promises";
import * as Bech32 from "../src/codec/bech32.js";
import { readPassword } from "./readPassword.js";

import * as Ncryptsec from "../src/crypto/ncryptsec.js"
import { signEvent } from "../src/sign/schnorr.js"

import { sha256 } from "@noble/hashes/sha2.js";

import { schnorr } from "@noble/secp256k1";

const [filePath, pkf, evf] = process.argv.slice(2);

const text = await readFile(filePath, "utf8");
const ev = JSON.parse(await readFile(evf, "utf8"));

const encrypted = Ncryptsec.decode(text);
const { dp: pk } = Bech32.decode((await readFile(pkf, "utf8")).trim());

const pswd = await readPassword();

const secretKey = await Ncryptsec.decrypt(encrypted, pswd);

console.log(ev);

const signed = await signEvent(ev, secretKey, pk);
console.log(signed);

const srlzd = JSON.stringify( [
	0, signed.pubkey, signed.created_at, signed.kind,
	signed.tags, signed.content] );
const hash = sha256(new TextEncoder().encode(srlzd));

console.log(Array.from(hash, b => b.toString(16).padStart(2, "0")).join(""));
console.log(signed.id);

console.log(await schnorr.verifyAsync(hexToBytes(signed.sig), hash, hexToBytes(signed.pubkey)));

function
hexToBytes(hex)
{
	const bs = new Uint8Array(hex.length / 2);
	for (let i = 0; i < bs.length; i++)
		bs[i] = parseInt(hex.slice(i * 2, i * 2 + 2), 16);
	return bs
}

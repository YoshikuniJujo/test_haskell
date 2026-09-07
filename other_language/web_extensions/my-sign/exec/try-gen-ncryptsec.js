import { mkdir, writeFile } from "node:fs/promises";
import { schnorr } from "@noble/secp256k1";
import { encode } from "../src/codec/bech32.js";
import { readPassword } from "./readPassword.js";
import * as Ncryptsec from "../src/crypto/ncryptsec.js";

const { secretKey: secKey, publicKey: pubKey } = schnorr.keygen();

await mkdir("key-pairs", { recursive: true });

const npub = encode("npub", pubKey);

const name = npub.slice(5, 15);

console.log(encode("npub", pubKey));

await writeFile(
	`key-pairs/${name}.npub`, npub + "\n",
	{ encoding: "utf8", flag: "wx" } );

const pswd = await readPassword();

const foo = await Ncryptsec.encrypt(
	secKey, { password: pswd, logN: 16, keySecurityByte: 1 } );

const ncryptsec = Ncryptsec.encode(foo);

await writeFile(`key-pairs/${name}.ncryptsec`,
	ncryptsec + "\n", { encoding: "utf8", mode: 0o600, flag: "wx" });

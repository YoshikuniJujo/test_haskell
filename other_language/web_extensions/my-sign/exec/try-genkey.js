import { mkdir, writeFile } from "node:fs/promises";
import { schnorr } from "@noble/secp256k1";
import { encode } from "../src/codec/bech32.js";

const { secretKey: secKey, publicKey: pubKey } = schnorr.keygen();

const nsec = encode("nsec", secKey);
const npub = encode("npub", pubKey);

const name = npub.slice(5, 15);

// console.log(encode("nsec", secKey));
console.log(encode("npub", pubKey));

await mkdir("key-pairs", { recursive: true });

await writeFile(
	`key-pairs/${name}.nsec`, nsec + "\n",
	{ encoding: "utf8", mode: 0o600, flag: "wx" } );

await writeFile(
	`key-pairs/${name}.npub`, npub + "\n",
	{ encoding: "utf8", flag: "wx" } );

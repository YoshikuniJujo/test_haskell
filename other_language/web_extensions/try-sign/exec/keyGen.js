import { writeFile, mkdir } from "node:fs/promises";
import { schnorr } from "@noble/secp256k1";
import { encode } from "../src/codec/bech32.js";

const { secretKey: secKey, publicKey: pubKey } = schnorr.keygen();

const nsec = encode("nsec", secKey);
const npub = encode("npub", pubKey);

const content = `export const nsec = "${nsec}";
export const npub = "${npub}";
`;

await mkdir("generated", {recursive: true });
await writeFile("generated/keyPair.js", content);

console.log({ nsec, npub });

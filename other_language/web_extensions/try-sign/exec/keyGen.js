import { schnorr } from "@noble/secp256k1";
import { encode } from "../src/codec/bech32.js";

const { secretKey: secKey, publicKey: pubKey } = schnorr.keygen();

const nsec = encode("nsec", secKey);
const npub = encode("npub", pubKey);

console.log(secKey);
console.log(pubKey);

console.log({ nsec, npub });

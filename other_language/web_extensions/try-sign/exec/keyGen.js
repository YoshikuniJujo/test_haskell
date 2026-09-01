import { schnorr } from "@noble/secp256k1"

const { secretKey: secKey, publicKey: pubKey } = schnorr.keygen();

console.log(secKey);
console.log(pubKey);

import { readFile } from "node:fs/promises";
import { readPassword } from "./readPassword.js";
import { webcrypto } from 'node:crypto';
import * as Bech32 from "../src/codec/bech32.js";

const filePath = process.argv[2];

const text = (await readFile(filePath, "utf8")).trim();

const { dp: secKey } = Bech32.decode(text);

console.log(secKey);

const salt = new Uint8Array(16);
const nonce = new Uint8Array(24);

webcrypto.getRandomValues(salt);
webcrypto.getRandomValues(nonce);

console.log(salt);
console.log(nonce);

const pswd = await readPassword();

console.log(pswd);

import { webcrypto } from 'node:crypto';
import { scrypt } from '@noble/hashes/scrypt.js';
import { xchacha20poly1305 } from '@noble/ciphers/chacha.js'
import { parse, format } from "node:path";
import { readFile, writeFile } from "node:fs/promises";
import * as Bech32 from "../src/codec/bech32.js";
import { readPassword } from "./readPassword.js";

const filePath = process.argv[2];

const p = parse(filePath);
const ncsfp = format({ dir: p.dir, name: p.name, ext: ".ncryptsec" });

const text = (await readFile(filePath, "utf8")).trim();

const { dp: secKey } = Bech32.decode(text);

const salt = new Uint8Array(16);
const nonce = new Uint8Array(24);

webcrypto.getRandomValues(salt);
webcrypto.getRandomValues(nonce);

const pswd = new TextEncoder().encode((await readPassword()).normalize("NFKC"));

const smkey = scrypt(pswd, salt, { N: 2 ** 16, r: 8, p: 1, dkLen: 32 });

const chacha = xchacha20poly1305(smkey, nonce, new Uint8Array([0]));
const encrypted = chacha.encrypt(secKey);

const ncryptsec = Bech32.encode('ncryptsec',
	new Uint8Array([2, 16, ...salt, ...nonce, 0, ...encrypted]));

console.log(ncryptsec);
console.log(ncsfp);

await writeFile(ncsfp,
	ncryptsec + "\n", { encoding: "utf8", mode: 0o600, flag: "wx" });

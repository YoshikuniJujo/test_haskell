import { parse, format } from "node:path";
import { readFile, writeFile } from "node:fs/promises";
import * as Bech32 from "../src/codec/bech32.js";
import { readPassword } from "./readPassword.js";

import * as Ncryptsec from "../src/crypto/ncryptsec.js";

const filePath = process.argv[2];

const p = parse(filePath);

const text = (await readFile(filePath, "utf8")).trim();

const { dp: secKey } = Bech32.decode(text);

const pswd = await readPassword();

const foo = await Ncryptsec.encrypt(
	secKey, { password: pswd, logN: 16, keySecurityByte: 0 } );

const ncryptsec = Bech32.encode('ncryptsec',
	new Uint8Array([
		foo.version, foo.logN, ...foo.salt, ...foo.nonce,
		foo.keySecurityByte, ...foo.ciphertext ]));

console.log(ncryptsec);

const ncsfp = format({ dir: p.dir, name: p.name, ext: ".ncryptsec" });
await writeFile(ncsfp,
	ncryptsec + "\n", { encoding: "utf8", mode: 0o600, flag: "wx" });

import { readFile } from "node:fs/promises";
import { scrypt } from "@noble/hashes/scrypt.js";
import { xchacha20poly1305 } from "@noble/ciphers/chacha.js";
import * as Bech32 from "../src/codec/bech32.js";
import { readPassword } from "./readPassword.js";

import * as Ncryptsec from "./ncryptsec.js"

const filePath = process.argv[2];

const text = await readFile(filePath, "utf8");

const encrypted = Ncryptsec.decode(text);

const pswd = await readPassword();

const secretKey = await Ncryptsec.decrypt(encrypted, pswd);

console.log(Bech32.encode('nsec', secretKey));

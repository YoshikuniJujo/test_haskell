import { readFile } from "node:fs/promises";
import * as Bech32 from "../src/codec/bech32.js";
import { readPassword } from "./readPassword.js";
import * as Ncryptsec from "../src/crypto/ncryptsec.js"
import { signEvent } from "../src/sign/schnorr.js"
import { verifyEvent } from "./verify.js";

const [ncsf, pkf, evf] = process.argv.slice(2);
const text = await readFile(ncsf, "utf8");
const ev = JSON.parse(await readFile(evf, "utf8"));

const encrypted = Ncryptsec.decode(text);

const { dp: pk } = Bech32.decode((await readFile(pkf, "utf8")).trim());

const pswd = await readPassword();

const secretKey = await Ncryptsec.decrypt(encrypted, pswd);

console.log(ev);

const signed = await signEvent(ev, secretKey, pk);
console.log(signed);

await verifyEvent(signed);

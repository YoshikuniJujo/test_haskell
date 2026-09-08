import { readFile } from "node:fs/promises";
import { readPassword } from "./readPassword.js";
import * as Ncryptsec from "../src/crypto/ncryptsec.js"
import { verifyEvent } from "./verify.js";

const [ncsf, pkf, evf] = process.argv.slice(2);
const text = await readFile(ncsf, "utf8");
const ev = JSON.parse(await readFile(evf, "utf8"));
const encrypted = Ncryptsec.decode(text);
const pswd = await readPassword();
const esk = await Ncryptsec.EncryptedSecretKey.fromEncrypted(encrypted, pswd);

console.log(await esk.checkPassword(pswd));
const smky = await esk.getSymmetricKey(pswd);
const signed = await esk.signEvent(ev, smky);
console.log(signed);
await verifyEvent(signed);

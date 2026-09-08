import { mkdir, writeFile } from "node:fs/promises";
import { encode } from "../src/codec/bech32.js";
import { readPassword } from "./readPassword.js";
import * as Ncryptsec from "../src/crypto/ncryptsec.js";

await mkdir("key-pairs", { recursive: true });
const pswd = await readPassword();

const esk = await Ncryptsec.EncryptedSecretKey.generate(pswd);
const obj = esk.toObject_563e7e39d4();
const np = encode("npub", esk.publicKey);
const nm = np.slice(5, 15);
const ncs = Ncryptsec.encode(obj);

const npfp = `key-pairs/${nm}.npub`;
const ncsfp = `key-pairs/${nm}.ncryptsec`;

console.log(ncsfp, npfp);

await writeFile(npfp, np + "\n", { encoding: "utf8", flag: "wx" });

await writeFile(
	ncsfp, ncs + "\n", { encoding: "utf8", mode: 0o600, flag: "wx" } );

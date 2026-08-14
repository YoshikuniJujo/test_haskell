import * as Bech32 from './bech32.js';
import * as Nip49 from './nip49.js';

console.log("foobar");

const npub = "npub12jnjezcvghm52ydev403rd3ndmrqxdjvunufjrkn3j02qzg4g7rqqung7n"
const ncryptsec =
	"ncryptsec1qggyeq0yg9ehxnfkt6yf7h4nurta9qtjdwcx76a8m08p" +
	"jqnxpd6cd67uqm5zsmk5420sq8at5luavqf88h98hvfzqegl3gaskj" +
	"rjz3fmcawze63lfjl2u7t9reqaqwur8rmqvmuarhfy5yxugvyedeg9"

console.log(npub);
console.log(ncryptsec);

const ncryptsec_unbech32 = Bech32.decode(ncryptsec);

console.log(ncryptsec_unbech32);

const [vsn, lgn, slt, nnc, aad, ct, mac] =
	Nip49.split(ncryptsec_unbech32, [1, 1, 16, 24, 1, 32, 16]);

console.log(vsn, lgn, slt, nnc, aad, ct, mac);

const symKeyPrms = { logN: lgn[0], salt: slt };
const encrypted = {
	version: vsn[0], nonce: nnc,
	keySecurityByte: aad[0], cipherText: ct, mac: mac };

console.log(symKeyPrms);
console.log(encrypted);

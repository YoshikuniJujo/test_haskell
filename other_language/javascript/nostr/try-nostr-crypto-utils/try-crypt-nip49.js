import readlineSync from 'readline-sync';
import { bech32 } from 'bech32';
import { nip49 } from 'nostr-crypto-utils';

import fs from 'fs';

const n = process.argv[2]

/*
const password = readlineSync.question('Password: ', {
	hideEchoBack: true,
	mask: ''
});
*/

const password = fs.readFileSync(`test-vectors/test${n}.password`).toString();

console.log(password);

const nsec = await fs.readFile(`test-vectors/test${n}.nsec`, (err, dt) => {
	const str = dt.toString();
	const decoded = bech32.decode(str).words;
	const secretKey = Uint8Array.from(bech32.fromWords(decoded));
	const ncryptsec = nip49.encrypt(secretKey, password);

	console.log(dt);
	console.log(str);
	console.log(decoded);
	console.log(ncryptsec);

	console.log(password);

	fs.writeFile(`test-vectors/test${n}.ncryptsec`, ncryptsec, (err) => { if(err) throw err; });

	const secretKey_ = nip49.decrypt(ncryptsec, password);

	console.log(secretKey_);

	});

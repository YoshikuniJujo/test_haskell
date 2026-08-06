import readlineSync from 'readline-sync';
import { bech32 } from 'bech32';
import { nip49 } from 'nostr-crypto-utils';

import fs from 'fs';

const password = readlineSync.question('Password: ', {
	hideEchoBack: true,
	mask: ''
});

const nsec = await fs.readFile('/home/tatsuya/tmp/foo00.nsec', (err, dt) => {
	const str = dt.toString();
	const decoded = bech32.decode(str).words;
	const secretKey = Uint8Array.from(bech32.fromWords(decoded));
	const ncryptsec = nip49.encrypt(secretKey, password);

	console.log(dt);
	console.log(str);
	console.log(decoded);
	console.log(ncryptsec);

	console.log(password);

	fs.writeFile('/home/tatsuya/tmp/foocrypt00.nsec', ncryptsec, (err) => { if(err) throw err; });

	const secretKey_ = nip49.decrypt(ncryptsec, password);

	console.log(secretKey_);

	});

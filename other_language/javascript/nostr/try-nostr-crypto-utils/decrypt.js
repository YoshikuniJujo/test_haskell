import { bech32 } from 'bech32';
import { nip49 } from 'nostr-crypto-utils';
import fs from 'fs';

const password = fs.readFileSync('/home/tatsuya/project/test_haskell/themes/nostr/nip49/try-nip49/test_vectors/test04.password').toString();


const ncryptsec = fs.readFileSync('/home/tatsuya/project/test_haskell/themes/nostr/nip49/try-nip49/test_vectors/test04.ncryptsec').toString();

console.log(password);
console.log(ncryptsec);

const secretKey = nip49.decrypt(ncryptsec, password);
const words = bech32.toWords(secretKey);
const encoded = bech32.encode('nsec', words, 1000);

console.log(secretKey);
console.log(encoded);

import fs from 'node:fs/promises';

console.log("UNSAFE FIXED SALT AND NONCE ENCRYPTER");

const buffer = await fs.readFile('../../../themes/nostr/nip49/try-nip49/test_vectors/test00.nsec');
const password = await fs.readFile('../../../themes/nostr/nip49/try-nip49/test_vectors/test00.password');
const text = new TextDecoder().decode(buffer);

console.log(text);

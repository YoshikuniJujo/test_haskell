import crypto from 'crypto';
import './bech32.js';

function hexToBytes(hexString) {
	const bytes = new Uint8Array(hexString.length / 2);

	for (let i = 0; i < bytes.length; i++) {
		bytes[i] = parseInt(hexString.substring(i * 2, i * 2 + 2), 16);
	}

	return bytes;
}

const password = 'nostr';

const log_n = 16;

const salt = crypto.randomBytes(16);

console.log(2 ** log_n);
console.log(2 ** 18);
const symmetric_key = crypto.scryptSync(password, salt, 32, {
	N: 2 ** log_n,
	r: 8, 
	p: 1,
	maxmem: 128 * 1024 * 1024
	});

const privateKey16 = "3501454135014541350145413501453fefb02227e449e57cf4d3a3ce05378683";

const privateKey = hexToBytes(privateKey16);

const key_security_byte = 0x01

console.log(symmetric_key);
console.log(privateKey);

console.log(digits.length);

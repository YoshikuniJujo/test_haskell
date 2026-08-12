import { scrypt } from '@noble/hashes/scrypt.js';
import { xchacha20poly1305 } from '@noble/ciphers/chacha.js';

function generate(ws) {
	return (1 ^ polymod([...ws, 0, 0, 0, 0, 0, 0])) >>> 0;
}

function verify(ws) {
	return polymod(ws) == 1;
}

const gen = [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3];
const mask = 0x3fffffff;

function applyGen(w5, cs) {
	for (let i = 0; i < 5; i++) {
		if ((w5 & (1 << i)) !== 0)
			cs ^= gen[i];
	}
	return (cs & mask);
}

function polymod(ws, cs = 1) {
	for (const w5 of ws) {
		const h5 = cs >>> 25;
		cs = ((cs << 5) |w5) >>> 0;
		cs = applyGen(h5, cs);
	}
	return (cs & mask) >>> 0;
}

function word30ToWord5List(w30) {
	return [
		(w30 >>> 25) & 0x1f,
		(w30 >>> 20) & 0x1f,
		(w30 >>> 15) & 0x1f,
		(w30 >>> 10) & 0x1f,
		(w30 >>> 5) & 0x1f,
		w30 & 0x1f
	];
}

const cs = generate([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
const ws2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, ...word30ToWord5List(cs)];

console.log(polymod([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
console.log(generate([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
console.log((word30ToWord5List(1061443723)));
console.log(cs)
console.log(ws2)
console.log(verify(ws2));

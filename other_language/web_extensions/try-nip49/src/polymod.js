import { scrypt } from '@noble/hashes/scrypt.js';
import { xchacha20poly1305 } from '@noble/ciphers/chacha.js';

export function generate(ws) {
	return (1 ^ polymod([...ws, 0, 0, 0, 0, 0, 0])) >>> 0;
}

export function verify(ws) {
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

export function word30ToWord5List(w30) {
	return [
		(w30 >>> 25) & 0x1f,
		(w30 >>> 20) & 0x1f,
		(w30 >>> 15) & 0x1f,
		(w30 >>> 10) & 0x1f,
		(w30 >>> 5) & 0x1f,
		w30 & 0x1f
	];
}

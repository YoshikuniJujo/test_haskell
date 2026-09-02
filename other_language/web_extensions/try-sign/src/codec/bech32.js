import { generate, verify } from './polymod.js';
import * as Word from './word.js';

export function encode(hrp, dp) {

	const c5 = chunks(5, Array.from(dp));
	const w40sInit = c5.init.map(Word.word8sToWord40);
	const w40Last = Word.word8sToWord40(c5.last) << 8n * (5n - BigInt(c5.lastN))
	const w40s = {
		init: w40sInit,
		last: w40Last,
		lastN: c5.lastN * 8 }
	const w5sInit = w40s.init.map(Word.word40ToWord5s);
	const w5sLast = Word.word40ToWord5s(w40s.last).slice(0, Math.ceil(w40s.lastN / 5));
	const w5s = w5sInit.flat().concat(w5sLast);
	const w5s2 = [...hrpExpand([...hrp]), ...w5s];
	const checksum = Word.word30ToWord5List(generate(w5s2));
	const w5s3 = [...w5s, ...checksum];

	return hrp + '1' + w5s3.map(w => charset[w]).join('');
}

export function decode(txt) {
	const chars = [...txt];
	const i = chars.lastIndexOf('1');
	const hrp = chars.slice(0, i);
	const dp = chars.slice(i + 1);
	const hrp5 = hrpExpand(hrp);
	const dp5 = dpToWord5s(dp);

	if (!verify([...hrp5, ...dp5])) {
		throw new Error('invalid checksum');
	}
	const c8 = chunks(8, dp5.slice(0, -6));
	const c40 = {
		init: c8.init.map(Word.word5sToWord40),
		last: Word.word5sToWord40(c8.last) << 5n * (8n - BigInt(c8.lastN)),
		lastN: c8.lastN * 5 }
	const dataPartInit = c40.init.map(Word.word40ToWord8List);
	const dataPartLast = Word.word40ToWord8ListTail(c40.last, c40.lastN / 8);
	const dataPart = new Uint8Array(dataPartInit.flat().concat(dataPartLast));
	return { humanReadable: hrp.join(''), data: dataPart }
}

function hrpExpand(hrp) {
	const bs = hrp.map(c => c.charCodeAt(0));
	return [
		...bs.map(b => b >>> 5),
		0,
		...bs.map(b => b & 0x1f)
	];
}

function chunks(n, xs) {
	const ln = xs.length;
	if (ln < n) {
		return { init: [], last: xs, lastN: ln };
	}
	const ys = chunks(n, xs.slice(n));
	const init = ys.init;
	const last = ys.last;
	const lastN = ys.lastN;
	return { init: [xs.slice(0, n), ...init], last, lastN };
}

const charset = 'qpzry9x8gf2tvdw0s3jn54khce6mua7l';

function dpToWord5s(dp) {
	return dp.map(c => charset.indexOf(c));
}

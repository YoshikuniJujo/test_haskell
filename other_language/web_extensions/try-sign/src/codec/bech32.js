import { generate, verify } from './polymod.js';
import * as Word from './word.js';

const charset = 'qpzry9x8gf2tvdw0s3jn54khce6mua7l';

export function
encode(hrp, dp)
{
	const c5 = chunks(5, Array.from(dp));

	const w40sInit = c5.init.map(Word.pack8sTo40);
	const w5sInit = w40sInit.map(Word.unpack40To5s);

	const w40Last = Word.pack8sTo40(c5.last) << 8n * (5n - BigInt(c5.lastN))
	const w5sLast = Word.unpack40To5s(w40Last).slice(0, Math.ceil(c5.lastN * 8 / 5));

	const w5s = w5sInit.flat().concat(w5sLast);

	const chksm = Word.unpack30To5s(generate([...hrpEx([...hrp]), ...w5s]));
	return hrp + '1' + [...w5s, ...chksm].map(w => charset[w]).join('');
}

export function
decode(txt)
{
	const chars = [...txt];
	const i = chars.lastIndexOf('1');
	const hrp = chars.slice(0, i);
	const dp = chars.slice(i + 1);
	const hrp5 = hrpEx(hrp);
	const dp5 = dp.map(c => charset.indexOf(c));
	if (!verify([...hrp5, ...dp5])) {
		throw new Error('invalid checksum');
	}
	const c8 = chunks(8, dp5.slice(0, -6));
	const c40 = {
		init: c8.init.map(Word.pack5sTo40),
		last: Word.pack5sTo40(c8.last) << 5n * (8n - BigInt(c8.lastN)),
		lastN: c8.lastN * 5 }
	const dataPartInit = c40.init.map(Word.unpack40To8s);
	const dataPartLast = Word.unpack40To8s(c40.last).slice(0, c40.lastN / 8);
		// Word.word40ToWord8ListTail(c40.last, c40.lastN / 8);
	const dataPart = new Uint8Array(dataPartInit.flat().concat(dataPartLast));
	return { humanReadable: hrp.join(''), data: dataPart }
}

function
hrpEx(hrp)
{
	const bs = hrp.map(c => c.charCodeAt(0));
	return [
		...bs.map(b => b >>> 5),
		0,
		...bs.map(b => b & 0x1f)
	];
}

function
chunks(sz, xs)
{
	const ln = xs.length;
	if (ln < sz) {
		return { init: [], last: xs, lastN: ln };
	}
	const ys = chunks(sz, xs.slice(sz));
	const init = ys.init;
	const last = ys.last;
	const lastN = ys.lastN;
	return { init: [xs.slice(0, sz), ...init], last, lastN };
}

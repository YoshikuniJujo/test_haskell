import { generate, verify } from './polymod.js';
import * as W from './word.js';

const charset = 'qpzry9x8gf2tvdw0s3jn54khce6mua7l';

export function
encode(hrp, dp)
{
	const { init: ci, last: cl, lastN: cn } = chunks(5, Array.from(dp));
	const w5i = ci.map(W.pack8sTo40).map(W.unpack40To5s);
	const w5l = W.unpack40To5s(W.pack8sTo40(cl) << 8n * (5n - BigInt(cn)))
		.slice(0, Math.ceil(cn * 8 / 5));
	const w5s = w5i.flat().concat(w5l);
	const chksm = W.unpack30To5s(generate([...hrpEx([...hrp]), ...w5s]));
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
		init: c8.init.map(W.pack5sTo40),
		last: W.pack5sTo40(c8.last) << 5n * (8n - BigInt(c8.lastN)),
		lastN: c8.lastN * 5 }
	const dataPartInit = c40.init.map(W.unpack40To8s);
	const dataPartLast = W.unpack40To8s(c40.last).slice(0, c40.lastN / 8);
		// W.word40ToWord8ListTail(c40.last, c40.lastN / 8);
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

import { generate, verify } from './polymod.js';
import * as W from './word.js';

const charset = 'qpzry9x8gf2tvdw0s3jn54khce6mua7l';

export function
encode(hrp, dp)
{
	const { init: ci, last: cl, lastN: cn } = chunks(5, Array.from(dp));
	const w5i = ci.map(W.pack8sTo40).map(W.unpack40To5s);
	const w5l = W.unpack40To5s(W.pack8sTo40(cl)).slice(0, ceil(cn * 8 / 5));
	const w5s = w5i.flat().concat(w5l);
	const chksm = W.unpack30To5s(generate([...hrpEx([...hrp]), ...w5s]));
	return hrp + '1' + [...w5s, ...chksm].map(w => charset[w]).join('');
}

export function
decode(txt)
{
	const cs = [...txt];
	const i = cs.lastIndexOf('1'); const hrp = cs.slice(0, i);
	const dp = cs.slice(i + 1).map(c => charset.indexOf(c));
	if (!verify([...hrpEx(hrp), ...dp])) throw new Error(chksmErr);
	const { init: ci, last: cl, lastN: cn } = chunks(8, dp.slice(0, -6));
	const w8i = ci.map(W.pack5sTo40).map(W.unpack40To8s);
	const w8l = unpad(W.unpack40To8s(W.pack5sTo40(cl)), cn * 5 / 8);
	return { hrp: hrp.join(''), dp: new Uint8Array(w8i.flat().concat(w8l)) }
}

function
unpad(xs, n)
{
	if (xs.slice(n).some(x => x !== 0)) throw new Error("invalid padding");
	return xs.slice(0, n);
}

const chksmErr = "invalid checksum";

function
hrpEx(hrp)
{
	const ns = hrp.map(c => c.charCodeAt(0));
	return [...ns.map(n => n >>> 5), 0, ...ns.map(n => n & 0x1f)];
}

function
chunks(sz, xs)
{
	const ln = xs.length;
	if (ln < sz) return { init: [], last: xs, lastN: ln };
	const { init: i, last: l, lastN: n } = chunks(sz, xs.slice(sz));
	return { init: [xs.slice(0, sz), ...i], last: l, lastN: n };
}

const ceil = Math.ceil;

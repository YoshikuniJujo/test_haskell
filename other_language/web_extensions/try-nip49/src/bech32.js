import { generate, verify, word30ToWord5List } from '../src/polymod.js';

function hrpExpand(hrp) {
	const bs = hrp.map(c => c.charCodeAt(0));
	return [
		...bs.map(b => b >>> 5),
		0,
		...bs.map(b => b & 0x1f)
	];
}

function split(bs, ns) {
	if (ns.length === 0) {
		return [];
	}

	const [n, ...rest] = ns;
	return [bs.slice(0, n), ...split(bs.slice(n), rest)];
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

function word5sToWord40(ws) {
	return ws.reduce(
		(w, x) => (w << 5n) | BigInt(x),
		0n
	);
}

function word40ToWord8List(w) {
	return [
		Number((w >> 32n) & 0xffn),
		Number((w >> 24n) & 0xffn),
		Number((w >> 16n) & 0xffn),
		Number((w >> 8n) & 0xffn),
		Number(w & 0xffn)
	];
}

function word40ToWord8ListTail(w, n) {
	return word40ToWord8List(w).slice(0, n);
}

export function bech32Decode(txt) {
	const chars = [...txt];
	const i = chars.lastIndexOf('1');
	const hrp = chars.slice(0, i);
	const dp = chars.slice(i + 1);
	const hrp5 = hrpExpand(hrp);
	const dp5 = dpToWord5s(dp);

	if (!verify([...hrp5, ...dp5])) {
		throw new Error('invalid checksum');
	}

	const decoded = {
		humanReadablePart: hrp.join(''),
		dataPart: dp5.slice(0, -6) }
	const c8 = chunks(8, decoded.dataPart);
	const c40 = {
		init: c8.init.map(word5sToWord40),
		last: word5sToWord40(c8.last) << 5n * (8n - BigInt(c8.lastN)),
		lastN: c8.lastN * 5 }
	const dataPartInit = c40.init.map(word40ToWord8List);
	const dataPartLast = word40ToWord8ListTail(c40.last, c40.lastN / 8);
	const dataPart = new Uint8Array(dataPartInit.flat().concat(dataPartLast));
	return dataPart;
}

export function bech32Encode(hrp, dp) {

	const secretKeyC5 = chunks(5, Array.from(dp));
	const secretKeyW40sInit = secretKeyC5.init.map(word8sToWord40);
	const secretKeyW40Last = word8sToWord40(secretKeyC5.last) << 8n * (5n - BigInt(secretKeyC5.lastN))
	const secretKeyW40s = {
		init: secretKeyW40sInit,
		last: secretKeyW40Last,
		lastN: secretKeyC5.lastN * 8 }
	const secretKeyW5sInit = secretKeyW40s.init.map(word40ToWord5s);
	const secretKeyW5sLast = word40ToWord5s(secretKeyW40s.last).slice(0, Math.ceil(secretKeyW40s.lastN / 5));
	const secretKeyW5s = secretKeyW5sInit.flat().concat(secretKeyW5sLast);
	const secretKeyW5s2 = [...hrpExpand([...hrp]), ...secretKeyW5s];
	const checksum = word30ToWord5List(generate(secretKeyW5s2));
	const secretKeyW5s3 = [...secretKeyW5s, ...checksum];
	const nsec = hrp + '1' + secretKeyW5s3.map(w => charset[w]).join('');

	return nsec;

}

function word8sToWord40(ws) {
	let w = 0n;

	for (const x of ws) {
		w = (w << 8n) | BigInt(x);
	}

	return w;
}

function word40ToWord5s(w) {
	return [
		Number((w >> 35n) &31n),
		Number((w >> 30n) &31n),
		Number((w >> 25n) &31n),
		Number((w >> 20n) &31n),
		Number((w >> 15n) &31n),
		Number((w >> 10n) &31n),
		Number((w >> 5n) &31n),
		Number(w & 31n) ];
}

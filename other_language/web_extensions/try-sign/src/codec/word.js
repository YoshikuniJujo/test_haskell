export function
pack5sTo40(ws)
{
	return ws.reduce((w, x) => (w << 5n) | BigInt(x), 0n);
}

export function
pack8sTo40(ws)
{
	return ws.reduce((w, x) => (w << 8n) | BigInt(x), 0n);
}

export function
unpack30To5s(w30)
{
	return [
		(w30 >>> 25) & 0x1f, (w30 >>> 20) & 0x1f, (w30 >>> 15) & 0x1f,
		(w30 >>> 10) & 0x1f, (w30 >>> 5) & 0x1f, w30 & 0x1f ];
}

export function
unpack40To5s(w)
{
	return [
		Number((w >> 35n) & 31n), Number((w >> 30n) & 31n),
		Number((w >> 25n) & 31n), Number((w >> 20n) & 31n),
		Number((w >> 15n) & 31n), Number((w >> 10n) & 31n),
		Number((w >> 5n) & 31n), Number(w & 31n) ];
}

export function unpack40To8s(w)
{
	return [
		Number((w >> 32n) & 0xffn), Number((w >> 24n) & 0xffn),
		Number((w >> 16n) & 0xffn), Number((w >> 8n) & 0xffn),
		Number(w & 0xffn) ];
}

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
unpack30To5s(w)
{
	return [
		extract(w, 25, 0x1f), extract(w, 20, 0x1f),
		extract(w, 15, 0x1f), extract(w, 10, 0x1f),
		extract(w, 5, 0x1f), extract(w, 0, 0x1f) ];
}

function
extract(w, s, m)
{
	return (w >>> s) & m;
}

export function
unpack40To5s(w)
{
	return [
		extractn(w, 35n, 0x1fn), extractn(w, 30n, 0x1fn),
		extractn(w, 25n, 0x1fn), extractn(w, 20n, 0x1fn),
		extractn(w, 15n, 0x1fn), extractn(w, 10n, 0x1fn),
		extractn(w, 5n, 0x1fn), extractn(w, 0n, 0x1fn) ];
}

export function unpack40To8s(w)
{
	return [
		extractn(w, 32n, 0xffn), extractn(w, 24n, 0xffn),
		extractn(w, 16n, 0xffn), extractn(w, 8n, 0xffn),
		extractn(w, 0n, 0xffn) ];
}

function
extractn(w, s, m)
{
	return Number((w >> s) & m);
}

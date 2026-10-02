export function
addToArrayMap<K, V>(map: Map<K, V[]>, k: K, v: V)
{
	const vs = map.get(k); if (vs) vs.push(v); else map.set(k, [v]);
}

export function
forEachValues<K, V>(map: Map<K, V[]>, k: K, f: (v: V) => void)
{
	const vs = map.get(k);
	if (vs === undefined) throw new Error("mapArray.ts: bad");
	map.delete(k); for (const v of vs) f(v);
}

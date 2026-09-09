export function
addToArrayMap(map, k, v)
{
	const vs = map.get(k); if (vs) vs.push(v); else map.set(k, [v]);
}

export function
forEachValues(map, k, f)
{
	const vs = map.get(k); map.delete(k); for (const v of vs) f(v);
}

export function
toHex(bytes: Uint8Array): string
{
	return Array.from(bytes, b => b.toString(16).padStart(2, "0")).join("");
}

export function
fromHex(hex: string): Uint8Array
{
	if (!/^[0-9a-fA-F]*$/.test(hex) || hex.length %2 !== 0)
		throw new Error("Invalid hex");

	const result = new Uint8Array(hex.length / 2);

	for (let i = 0; i < result.length; i++)
		result[i] = parseInt(hex.slice(i * 2, i * 2 + 2), 16);

	return result;
}

export function
isUuid(value: string): boolean
{
	return /^[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/i.test(value);
}

export function
isUrl(s: string)
{
	try { new URL(s); return true; } catch { return false; }
}

export function
encodePassword(pswd: string): Uint8Array
{
	return new TextEncoder().encode(pswd.normalize("NFKC"));
}

export function
findKey<K, V>(map: Map<K, V>, p: (value: V) => boolean): K | undefined
{
	for (const [k, v] of map) if (p(v)) return k;
	return undefined;
}

export function
camelToSnake(s: string): string
{
	return go2(true, s);

	function
	go(s: string): string
	{
		if (s === "") return "";
		const [c, ...cs] = s;
		if (c === undefined) throw new Error("never occur");
		if (/[A-Z]/.test(c))
			return "_" +
				allowChar(c.toLowerCase()) +
				go2(false, cs.join(""));
		return allowChar(c) + go(cs.join(""));
	}

	function
	go2(h: boolean, s: string): string
	{
		if (s === "") return "";
		if (s.length === 1) return s.toLowerCase();

		const [c, ...cs] = s;
		const d = cs[0];

		if (c === undefined) throw new Error("never occur");
		if (d === undefined) throw new Error("never occur");
		if (/[A-Z]/.test(c) && /[A-Z]/.test(d))
			return allowChar(c.toLowerCase()) +
				go2(false, cs.join(""));

		if (/[A-Z]/.test(c) && /[a-z]/.test(d))
			return (h ? "" : "_") +
				allowChar(c.toLowerCase()) + go(cs.join(""));

		return allowChar(c.toLowerCase()) + go(cs.join(""));
	}

	function
	allowChar(c: string): string
	{
		return /^[a-z0-9_]$/.test(c) ? c : "";
	}
}

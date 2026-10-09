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
	return /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(value);
}

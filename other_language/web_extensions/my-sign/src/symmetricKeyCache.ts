export async function
get(pk: string): Promise<Uint8Array | undefined>
{
	const pswds: Record<string, Uint8Array> =
		(await browser.storage.session.get("pswds")).pswds ?? {};
	return pswds[pk];
}

export async function
set(pk: string, key: Uint8Array): Promise<void>
{
	const pswds: Record<string, Uint8Array> =
		(await browser.storage.session.get("pswds")).pswds ?? {};
	pswds[pk] = key;
	await browser.storage.session.set({ pswds: pswds });
}

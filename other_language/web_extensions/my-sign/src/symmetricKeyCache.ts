import { Mutex } from "./mutex.js"

const mutex = new Mutex();

export async function
get(pk: string): Promise<Uint8Array | undefined>
{
	await mutex.acquire();
	try {
		const pswds: Record<string, Uint8Array> =
			(await browser.storage.session.get("pswds")).pswds ?? {};
		return pswds[pk]; }
	finally { mutex.release(); }
}

export async function
set(pk: string, key: Uint8Array): Promise<void>
{
	await mutex.acquire();
	try {
		const pswds: Record<string, Uint8Array> =
			(await browser.storage.session.get("pswds")).pswds ?? {};
		pswds[pk] = key;
		await browser.storage.session.set({ pswds: pswds }); }
	finally { mutex.release(); }
}

export async function
dlt(pk: string): Promise<void>
{
	await mutex.acquire();
	try {
		const pswds: Record<string, Uint8Array> =
			(await browser.storage.session.get("pswds")).pswds ?? {};
		delete pswds[pk];
		await browser.storage.session.set({ pswds: pswds }); }
	finally { mutex.release(); }
}

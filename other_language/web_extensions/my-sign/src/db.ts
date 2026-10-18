import type {
	Client, ClientSummary, EditingClient,
	Account, PublicKeyOption, UUID } from "./types.js"
import { fromUuid } from "./types.js"
import { unit } from "./data/unit.js"
import { toHex } from "./tools.js"

declare const __DEV__: boolean;

const DB_NAME = "my-sign";
const DB_VERSION = 4;

const SECRET_KEYS = "secret-keys";
const CLIENTS = "setClients";
const OLD_CLIENTS = "clients";
const USE_CLIENT_SET = "use-client-set";

let dbPromise: Promise<IDBDatabase> | undefined;

function
open()
{
	if (dbPromise) return dbPromise;

	console.log("db.ts: open");

	dbPromise = new Promise((rs, rj) => {
		const req = indexedDB.open(DB_NAME, DB_VERSION);
		req.onupgradeneeded = (event) => {
			const db = req.result;
			const tx = req.transaction!;

			if (!db.objectStoreNames.contains(SECRET_KEYS))
				db.createObjectStore(
					SECRET_KEYS, { keyPath: "publicKey" } );

			if (!db.objectStoreNames.contains(OLD_CLIENTS))
				db.createObjectStore(
					OLD_CLIENTS, { keyPath: "uuid" } );

			if (!db.objectStoreNames.contains(USE_CLIENT_SET))
				db.createObjectStore(USE_CLIENT_SET);


			if (event.oldVersion < 3) {
				const oldStore = tx.objectStore(OLD_CLIENTS);
				const newStore = db.createObjectStore(
					"setClients",
					{ keyPath: "uuid" }
				);

				oldStore.getAll().onsuccess = (event) => {
					const records = (event.target as IDBRequest).result;

					for (const record of records) {
						newStore.add(record);
					}
					db.deleteObjectStore(OLD_CLIENTS);
				};
			}

			if (event.oldVersion < 4) {
				db.deleteObjectStore(OLD_CLIENTS);
				console.log("db.ts: TO VERSION 4");
				const setClients = tx.objectStore("setClients");
				const newStore = db.createObjectStore(
					"clients", { keyPath: "uuid" });
				setClients.getAll().onsuccess = event => {
					const records = (event.target as IDBRequest).result;
					for (const record of records) {
						newStore.add({
							uuid: record.uuid,
							name: record.name,
							urlPattern: record.urlPattern
						});
					}
				};
			}
		};

		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});

	return dbPromise;
}

export async function
addKeyPair(key: Account)
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(SECRET_KEYS, "readwrite");
		const store = tx.objectStore(SECRET_KEYS);
		store.add(key);
		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error);
	});
}

export async function
getPublicKeys()
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(SECRET_KEYS, "readonly");
		const store = tx.objectStore(SECRET_KEYS);
		const req = store.getAllKeys();

		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});
}

export async function
putClient(client: Client)
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(CLIENTS, "readwrite");
		tx.objectStore(CLIENTS).put(client);

		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error);
	});
}

export async function
deleteClient(uuid: UUID)
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(CLIENTS, "readwrite");
		tx.objectStore(CLIENTS).delete(fromUuid(uuid));
		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error);
	});
}

type KeyWithName = {
	publicKey: Uint8Array,
	name: string }

export async function
getPublicKeysWithNames(): Promise<KeyWithName[]>
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(SECRET_KEYS, "readonly");
		const store = tx.objectStore(SECRET_KEYS);
		const req = store.getAll();

		req.onsuccess = () => rs(req.result.map(key => ({
			publicKey: key.publicKey,
			name: key.name })));
		req.onerror = () => rj(req.error);
	});
}

export async function
getPublicKeyOptions(): Promise<PublicKeyOption[]>
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(SECRET_KEYS, "readonly");
		const store = tx.objectStore(SECRET_KEYS);
		const req = store.getAll();
		/*
		const rslt = req.result;
		const srslt = [...rslt].sort(
			(a, b) => a.name.localeCompare(b.name)
		);
		*/

		req.onsuccess = () => {
			const rslt = req.result;
			const srslt = [...rslt].sort(
				(a, b) => a.name.localeCompare(b.name)
			);
			rs(srslt.map(k => ({
				type: "PublicKeyOption",
				publicKey: toHex(k.publicKey),
				name: k.name })));
		}
		req.onerror = () => rj(req.error);
	});
}

export async function
getClients(): Promise<Client[]>
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(CLIENTS, "readonly");
		const req = tx.objectStore(CLIENTS).getAll();

		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});
}

export async function
getClientSummaries(): Promise<ClientSummary[]>
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(CLIENTS, "readonly");
		const req = tx.objectStore(CLIENTS).getAll();

		req.onsuccess = () => {
			const rslts : Client[] = req.result;
			console.log(rslts);
			const srslts = [...rslts].sort(
				(a, b) => a.name?.localeCompare(b.name)
			);
			rs(srslts.map(r => {
				return {
					type: "ClientSummary",
					uuid: { type: "UUID", value: r.uuid },
					name: r.name, urlPattern: r.urlPattern } })) };
		req.onerror = () => rj(req.error);
	});
}

export async function
getClient(uuid: string): Promise<EditingClient>
{
	const db = await open()

	return new Promise((rs, rj) => {
		const tx = db.transaction(CLIENTS, "readonly");
		const req = tx.objectStore(CLIENTS).get(uuid);

		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});
}

export async function
getAccount(pk: Uint8Array): Promise<Account>
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(SECRET_KEYS, "readonly");
		const store = tx.objectStore(SECRET_KEYS);
//		const req = store.get(pk.slice().buffer);
		const req = store.get(pk as IDBValidKey);
		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});
}

export async function
getUseClientSet(): Promise<boolean>
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(USE_CLIENT_SET, "readonly");
		const store = tx.objectStore(USE_CLIENT_SET);
		const req = store.get(unit);
		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});
}

export async function
putUseClientSet(b: boolean)
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(USE_CLIENT_SET, "readwrite");
		tx.objectStore(USE_CLIENT_SET).put(b, unit);
		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error);
	});
}

export async function
resetDatabase()
{
	if (!__DEV__) throw new Error("Development build only");

	const db = await open();
	db.close();
	dbPromise = undefined

	await new Promise<void>((rs, rj) => {

		const request = indexedDB.deleteDatabase("my-sign");
		console.log("db.ts: resetDatabase run");

		request.onsuccess = () => {
			console.log("Database deleted");
			rs();
		};

		request.onerror = () => {
			console.error("Database deletion failed", request.error);
			rj(request.error);
		};

		request.onblocked = () => {
			console.warn("Database deletion is blocked");
		};

	});
}

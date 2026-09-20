import { unit } from "./data/unit.js"

const DB_NAME = "my-sign";
const DB_VERSION = 2;

const SECRET_KEYS = "secret-keys";
const CLIENTS = "clients";
const USE_CLIENT_SET = "use-client-set";

let dbPromise;

function
open()
{
	if (dbPromise) return dbPromise;

	dbPromise = new Promise((rs, rj) => {
		const req = indexedDB.open(DB_NAME, DB_VERSION);
		req.onupgradeneeded = () => {
			const db = req.result;

			if (!db.objectStoreNames.contains(SECRET_KEYS))
				db.createObjectStore(
					SECRET_KEYS, { keyPath: "publicKey" } );

			if (!db.objectStoreNames.contains(CLIENTS))
				db.createObjectStore(
					CLIENTS, { keyPath: "uuid" } );

			if (!db.objectStoreNames.contains(USE_CLIENT_SET))
				db.createObjectStore(USE_CLIENT_SET);
		};

		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});

	return dbPromise;
}

export async function
addKeyPair(key)
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
putClient(client)
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
deleteClient(uuid)
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(CLIENTS, "readwrite");
		tx.objectStore(CLIENTS).delete(uuid);
		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error);
	});
}

export async function
getPublicKeysWithNames()
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
getClients()
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
getAccount(pk)
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(SECRET_KEYS, "readonly");
		const store = tx.objectStore(SECRET_KEYS);
		const req = store.get(pk);
		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});
}

export async function
getUseClientSet()
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
putUseClientSet(b)
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(USE_CLIENT_SET, "readwrite");
		tx.objectStore(USE_CLIENT_SET).put(b, unit);
		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error);
	});
}

const DB_NAME = "my-sign";
const DB_VERSION = 1;

const SECRET_KEYS = "secret-keys";
const CLIENTS = "clients";

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

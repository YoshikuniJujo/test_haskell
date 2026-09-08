const DB_NAME = "my-sign";
const DB_VERSION = 1;
const STORE_NAME = "secret-keys";

let dbPromise;

function
open()
{
	if (dbPromise) return dbPromise;

	dbPromise = new Promise((rs, rj) => {
		const req = indexedDB.open(DB_NAME, DB_VERSION);
		req.onupgradeneeded = () => {
			const db = req.result;

			if (!db.objectStoreNames.contains(STORE_NAME))
				db.createObjectStore(
					STORE_NAME, { keyPath: "publicKey" });
		};

		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});

	return dbPromise;
}

export async function
add(key)
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(STORE_NAME, "readwrite");
		const store = tx.objectStore(STORE_NAME);
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
		const tx = db.transaction(STORE_NAME, "readonly");
		const store = tx.objectStore(STORE_NAME);
		const req = store.getAllKeys();

		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});
}

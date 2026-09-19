const DB_NAME = "my-sign-log";
const DB_VERSION = 1;
const STORE_NAME = "log";

let dbPromise;

function
open()
{
	return new Promise((rs, rj) => {
		const req = indexedDB.open(DB_NAME, DB_VERSION);
		req.onupgradeneeded = () => {
			if (!req.result.objectStoreNames.contains(STORE_NAME))
				req.result.createObjectStore(STORE_NAME, {
					autoIncrement: true }); };
		req.onsuccess = () => { rs(req.result); };
		req.onerror = () => { rj(req.error); };
	});
}

export async function
write(msg)
{
	const db = await getDB();
	const tx = db.transaction(STORE_NAME, "readwrite");
	const store = tx.objectStore(STORE_NAME);

	console.log("LOG: msg =", msg);

	store.add({
		time: Date.now(),
		message: msg });

	return new Promise((rs, rj) => {
		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error);
	});
}

export async function
readAll()
{
	const db = await getDB();
	const tx = db.transaction(STORE_NAME, "readonly");
	const store = tx.objectStore(STORE_NAME);

	return new Promise((rs, rj) => {
		const req = store.getAll();
		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});
}

function
getDB()
{
	if (!dbPromise) dbPromise = open();
	return dbPromise;
}

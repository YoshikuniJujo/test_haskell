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

type Log = { time: number, message: string }

const DB_NAME = "my-sign-log";
const DB_VERSION = 1;
const STORE_NAME = "log";

export async function
toString(): Promise<string>
{
	return (await readAll())
		.map(l => new Date(l.time).toISOString() + " " + l.message)
		.join("\n");
}

async function
readAll(): Promise<Log[]>
{
	const tx = (await getDB()).transaction(STORE_NAME, "readonly");
	const st = tx.objectStore(STORE_NAME);
	const req = st.getAll();
	return new Promise((rs, rj) => {
		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error); });
}

export async function
write(msg: string): Promise<void>
{
	const tx = (await getDB()).transaction(STORE_NAME, "readwrite");
	const st = tx.objectStore(STORE_NAME);
	st.add({ time: Date.now(), message: msg });
	return new Promise((rs, rj) => {
		tx.oncomplete = () => {
			void browser.runtime.sendMessage({
				class: "Options", direction: "signal",
				method: "logUpdated" }).catch(() => {});
			rs(); };
		tx.onerror = () => rj(tx.error);
		tx.onabort = () => rj(tx.error); });
}

let dbPromise: Promise<IDBDatabase>;

function
getDB()
{
	if (!dbPromise) dbPromise = open(); return dbPromise;
}

function
open()
{
	const req = indexedDB.open(DB_NAME, DB_VERSION);
	req.onupgradeneeded = () => {
		const db = req.result;
		if (!db.objectStoreNames.contains(STORE_NAME))
			db.createObjectStore(
				STORE_NAME, { autoIncrement: true } ); };
	return new Promise<IDBDatabase>((rs, rj) => {
		req.onsuccess = () => { rs(req.result); };
		req.onerror = () => { rj(req.error); }; });
}

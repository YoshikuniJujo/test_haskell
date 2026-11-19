type Log = { time: number, message: string }

const DB_NAME = "my-sign-log";
const DB_VERSION = 2;
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
	const now = Date.now();
	st.add({ time: now, message: msg });

	const limit = now - 90 * 24 * 60 * 60 * 1000;
	const idx = st.index("time");
	const range = IDBKeyRange.upperBound(limit);
	const req = idx.openCursor(range);
	req.onsuccess = () => {
		const cursor = req.result;
		if (cursor) {
			cursor.delete();
			cursor.continue();
		}
	};

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
	req.onupgradeneeded = ev => {
		const db = req.result;
		const tx = req.transaction!;
		if (ev.oldVersion < 1) updatev0v1(db, tx);
		else if (ev.oldVersion < 2) updatev1v2(db, tx);
		tx.oncomplete = () => {
			console.log("migration transaction complete") }; };
	return new Promise<IDBDatabase>((rs, rj) => {
		req.onsuccess = () => { rs(req.result); };
		req.onerror = () => { rj(req.error); }; });
}

function
updatev0v1(db: IDBDatabase, tx: IDBTransaction)
{
	db.createObjectStore(STORE_NAME, { autoIncrement: true });
	updatev1v2(db, tx);
}

function
updatev1v2(db: IDBDatabase, tx: IDBTransaction)
{
	tx.objectStore(STORE_NAME).createIndex("time", "time");
}

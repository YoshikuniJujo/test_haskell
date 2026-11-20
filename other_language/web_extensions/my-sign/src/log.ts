import type { Unit } from "./data/unit.js";
import { unit } from "./data/unit.js";

type Log = { time: number, message: string }

const DB_NAME = "my-sign-log";
const DB_VERSION = 3;
const STORE_NAME = "log";
const COUNTER = "couner";

const LIMIT_LENGTH = 50000;

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
	await deleteExpired();
	await add(msg);
	await makeRoom();
	void browser.runtime.sendMessage({
		class: "Options", direction: "signal",
		method: "logUpdated" }).catch(() => {});
}

async function
add(msg: string): Promise<void>
{
	const tx =
		(await getDB()).transaction([STORE_NAME, COUNTER], "readwrite");
	const completed = new Promise<void>((rs, rj) => {
		tx.oncomplete = () => rs();
		tx.onerror = () => rj(tx.error);
		tx.onabort = () => rj(tx.error); });
	const st = tx.objectStore(STORE_NAME);
	const ct = tx.objectStore(COUNTER);
	const now = Date.now();
	st.add({ time: now, message: msg });
	await incCounter(ct, msg.length);
	await completed;
}

async function
deleteExpired(): Promise<void>
{
	const tx =
		(await getDB()).transaction([STORE_NAME, COUNTER], "readwrite");
	const completed = new Promise<void>((rs, rj) => {
		tx.oncomplete = () => { rs(); };
		tx.onerror = () => rj(tx.error);
		tx.onabort = () => rj(tx.error); });
	const idx = tx.objectStore(STORE_NAME).index("time");
	const ct = tx.objectStore(COUNTER);
	const req = idx.openCursor(IDBKeyRange.upperBound(
		Date.now() - 90 * 24 * 60 * 60 * 1000 ));
	let dln = 0;
	req.onsuccess = async () => {
		const crsr = req.result;
		if (crsr) { dln += deleteL(crsr); crsr.continue(); }
		else await decCounter(ct, dln); };
	await completed;
}

async function
makeRoom(): Promise<void>
{
	const tx =
		(await getDB()).transaction([STORE_NAME, COUNTER], "readwrite");
	const completed = new Promise<void>((rs, rj) => {
		tx.oncomplete = () => { rs(); };
		tx.onerror = () => rj(tx.error);
		tx.onabort = () => rj(tx.error); });
	const ct = tx.objectStore(COUNTER);
	let length = await reqToPromise(ct.get("unit")) ?? 0;
	while (length > LIMIT_LENGTH) {
		const dln = await deleteOldest(tx, 200);
		if (dln === 0) break;
		await decCounter(ct, dln);
		length -= dln; }
	await completed;
}

async function
deleteOldest(tx: IDBTransaction, cnt: number): Promise<number>
{
	const idx = tx.objectStore(STORE_NAME).index("time");
	const req = idx.openCursor();
	return new Promise((rs, rj) => {
		let deleted = 0;
		let dln = 0;
		req.onsuccess = () => {
			const crsr = req.result;
			if (crsr && deleted < cnt) {
				dln += deleteL(crsr);
				deleted++;
				crsr.continue(); }
			else rs(dln); };
		req.onerror = () => rj(req.error); });

}

async function
incCounter(ct: IDBObjectStore, am: number)
{
	await reqToPromise(
		ct.put((await reqToPromise(ct.get("unit")) ?? 0) + am, "unit"));
}

async function
decCounter(ct: IDBObjectStore, am: number)
{
	await incCounter(ct, - am);
}

function
reqToPromise<T>(request: IDBRequest<T>): Promise<T>
{
	return new Promise((rs, rj) => {
		request.onsuccess = () => rs(request.result);
		request.onerror = () => rj(request.error);
	});
}

function
deleteL(cursor: IDBCursorWithValue): number
{
	cursor.delete();
	return cursor.value.message.length;
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
		else if (ev.oldVersion < 3) updatev2v3(db, tx);
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
	updatev2v3(db, tx);
}

function
updatev2v3(db: IDBDatabase, tx: IDBTransaction)
{
//	db.createObjectStore(COUNTER, { keyPath: "unit" });
	db.createObjectStore(COUNTER);
}

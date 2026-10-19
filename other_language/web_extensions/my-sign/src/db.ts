import type {
	Client, ClientSummary, ClientSummaryRaw, EditingClient,
	Account, PublicKeyOption, UUID } from "./types.js"
import { fromUuid } from "./types.js"
import { unit } from "./data/unit.js"
import { toHex } from "./tools.js"

declare const __DEV__: boolean;

const DB_NAME = "my-sign";
const DB_VERSION = 4;

const SECRET_KEYS = "secret-keys";
const SET_CLIENTS = "setClients";
const OLD_CLIENTS = "clients";
const NEW_CLIENTS = "clients";
const USE_CLIENT_SET = "use-client-set";

let dbPromise: Promise<IDBDatabase> | undefined;

function
open()
{
	if (dbPromise) return dbPromise;

	dbPromise = new Promise((rs, rj) => {
		const req = indexedDB.open(DB_NAME, DB_VERSION);

		req.onupgradeneeded = ev => {
			const db = req.result;
			const tx = req.transaction!;
			if (ev.oldVersion < 1) updatev0v1(db, tx);
			else if (ev.oldVersion < 3) updatev2v3(db, tx);
			else if (ev.oldVersion < 4) updatev3v4(db, tx); };
		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});

	return dbPromise;
}

function
updatev0v1(db: IDBDatabase, tx: IDBTransaction)
{
	db.createObjectStore(SECRET_KEYS, { keyPath: "publicKey" });
	db.createObjectStore(OLD_CLIENTS, { keyPath: "uuid" });
	updatev1v2(db, tx);
}

function
updatev1v2(db: IDBDatabase, tx: IDBTransaction)
{
	db.createObjectStore(USE_CLIENT_SET);
	updatev2v3(db, tx);
}

function
updatev2v3(db: IDBDatabase, tx: IDBTransaction)
{
	const oldCls = tx.objectStore(OLD_CLIENTS);
	const newCls = db.createObjectStore(SET_CLIENTS, { keyPath: "uuid" });

	oldCls.getAll().onsuccess = ev => {
		const rs = (ev.target as IDBRequest).result;
		for (const r of rs) newCls.add(r);
		db.deleteObjectStore(OLD_CLIENTS);
		updatev3v4(db, tx); };
}

function
updatev3v4(db: IDBDatabase, tx: IDBTransaction)
{
	const setClients = tx.objectStore(SET_CLIENTS);
	const newSt = db.createObjectStore(NEW_CLIENTS, { keyPath: "uuid" });

	setClients.getAll().onsuccess = ev => {
		const rs = (ev.target as IDBRequest).result;
		for (const r of rs) {
			newSt.add({
				uuid: r.uuid, name: r.name,
				urlPattern: r.urlPattern }); } };
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
		const tx = db.transaction([SET_CLIENTS, NEW_CLIENTS], "readwrite");
		tx.objectStore(SET_CLIENTS).put(client);
		tx.objectStore(NEW_CLIENTS).put({
			uuid: client.uuid,
			name: client.name,
			urlPattern: client.urlPattern });

		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error);
	});
}

export async function
deleteClient(uuid: UUID)
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(
			[SET_CLIENTS, NEW_CLIENTS], "readwrite" );
		tx.objectStore(SET_CLIENTS).delete(fromUuid(uuid));
		tx.objectStore(NEW_CLIENTS).delete(fromUuid(uuid));
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
		const tx = db.transaction(
			[SET_CLIENTS, NEW_CLIENTS], "readonly");
		const req = tx.objectStore(NEW_CLIENTS).getAll();

		req.onsuccess = () => {
			const rslts: Client[] = [];
			console.log(req.result);
			for (const r of req.result) {
				const req2 = tx.objectStore(SET_CLIENTS).get(r.uuid);
				req2.onsuccess = () => {
					const r2 = req2.result;
					rslts.push({
						uuid: r.uuid,
						name: r.name,
						urlPattern: r.urlPattern,
						publicKey: r2.publicKey,
						priority: r2.priority,
						openSettingsByClick: r2.openSettingsByClick,
						backgroundColor: r2.backgroundColor,
						backgroundOpacity: r2.backgroundOpacity,
						positionX: r2.positionX,
						positionY: r2.positionY,
						displayAccount: r2.displayAccount
					});
				};
				console.log("db.ts: getClients:", rslts);
				tx.oncomplete = () => {
					rs(rslts);
				}
			}
		}
		req.onerror = () => rj(req.error);
	});
}

export async function
getClientSummaries(): Promise<ClientSummary[]>
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(NEW_CLIENTS, "readonly");
		const req = tx.objectStore(NEW_CLIENTS).getAll();

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
		const tx = db.transaction(
			[SET_CLIENTS, NEW_CLIENTS], "readonly");
		const req = tx.objectStore(SET_CLIENTS).get(uuid);
		const req2 = tx.objectStore(NEW_CLIENTS).get(uuid);

		req.onsuccess = () => {
			req2.onsuccess = () => {
				const r = req.result;
				const r2 = req2.result
				rs({
					uuid: r2.uuid,
					name: r2.name,
					urlPattern: r2.urlPattern,
					publicKey: r.publicKey,
					priority: r.priority,
					openSettingsByClick:
						r.openSettingsByClick,
					backgroundColor: r.backgroundColor,
					backgroundOpacity:
						r.backgroundOpacity,
					positionX: r.positionX,
					positionY: r.positionY,
					displayAccount: r.displayAccount
				});
			}
		}
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

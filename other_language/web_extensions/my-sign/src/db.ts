import type {
	ClientSet, Client, ClientSummary, ClientSummaryRaw, EditingClient,
	Account, PublicKeyOption, UUID, Profile, Permission } from "./types.js"
import { fromUuid, noPermission, setPermissionSign } from "./types.js"
import { unit } from "./data/unit.js"
import { toHex } from "./tools.js"

declare const __DEV__: boolean;

const DB_NAME = "my-sign";
const DB_VERSION = 16;

const SECRET_KEYS = "secret-keys";
const OLD_SET_CLIENTS = "setClients";
const TMP_SET_CLIENTS = "setClientsTmp";
const NEW_SET_CLIENTS = "set-clients";
const OLD_CLIENTS = "clients";
const NEW_CLIENTS = "clients";
const USE_CLIENT_SET = "use-client-set";
const CURRENT_SET = "current-set";

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
			else if (ev.oldVersion < 2) updatev1v2(db, tx);
			else if (ev.oldVersion < 3) updatev2v3(db, tx);
			else if (ev.oldVersion < 4) updatev3v4(db, tx);
			else if (ev.oldVersion < 5) updatev4v5(db, tx);
			else if (ev.oldVersion < 6) updatev5v6(db, tx);
			else if (ev.oldVersion < 7) updatev6v7(db, tx);
			else if (ev.oldVersion < 8) updatev7v8(db, tx);
			else if (ev.oldVersion < 9) updatev8v9(db, tx);
			else if (ev.oldVersion < 10) updatev9v10(db, tx);
			else if (ev.oldVersion < 11) updatev10v11(db, tx);
			else if (ev.oldVersion < 12) updatev11v12(db, tx);
			else if (ev.oldVersion < 13) updatev12v13(db, tx);
			else if (ev.oldVersion < 14) updatev13v14(db, tx);
			else if (ev.oldVersion < 15) updatev14v15(db, tx);
			else if (ev.oldVersion < 16) updatev15v16(db, tx);
			tx.oncomplete = () => {
				console.log("migration transaction complete");
			}
		};
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
	const newCls = db.createObjectStore(OLD_SET_CLIENTS, { keyPath: "uuid" });

	oldCls.getAll().onsuccess = ev => {
		const rs = (ev.target as IDBRequest).result;
		for (const r of rs) newCls.add(r);
		db.deleteObjectStore(OLD_CLIENTS);
		updatev3v4(db, tx); };
}

function
updatev3v4(db: IDBDatabase, tx: IDBTransaction)
{
	const setClients = tx.objectStore(OLD_SET_CLIENTS);
	const newSt = db.createObjectStore(NEW_CLIENTS, { keyPath: "uuid" });

	setClients.getAll().onsuccess = ev => {
		const rs = (ev.target as IDBRequest).result;
		for (const r of rs) {
			newSt.add({
				uuid: r.uuid, name: r.name,
				urlPattern: r.urlPattern }); }
		updatev4v5(db, tx); };
}

function
updatev4v5(db: IDBDatabase, tx: IDBTransaction)
{
	const setClients = tx.objectStore(OLD_SET_CLIENTS);
	setClients.getAll().onsuccess = ev => {
		const rs = (ev.target as IDBRequest).result;
		for (const r of rs) {
			delete r.name;
			delete r.urlPattern;
			setClients.put(r);
		}
		updatev5v6(db, tx);
	}
}

function
updatev5v6(db: IDBDatabase, tx: IDBTransaction)
{
	const oldStore = tx.objectStore(OLD_SET_CLIENTS);
	const newStore = db.createObjectStore(TMP_SET_CLIENTS, {
		keyPath: "uuid"
	});

	const req = oldStore.getAll();

	req.onsuccess = () => {
		for (const r of req.result) {
			console.log("db.ts: updatev5v6:", r);
			newStore.add(r);
		}
		db.deleteObjectStore(OLD_SET_CLIENTS);
		updatev6v7(db, tx);
	};

}

function
updatev6v7(db: IDBDatabase, tx: IDBTransaction)
{
	db.createObjectStore("sets", { keyPath: "uuid" });
	updatev7v8(db, tx);
}

function
updatev7v8(db: IDBDatabase, tx: IDBTransaction)
{
	db.createObjectStore(CURRENT_SET);
	updatev8v9(db, tx);
}

function
updatev8v9(db: IDBDatabase, tx: IDBTransaction)
{
	initializeSet(tx);
	updatev9v10(db, tx);
}

function
updatev9v10(db: IDBDatabase, tx: IDBTransaction)
{
	db.createObjectStore(NEW_SET_CLIENTS, {
		keyPath: ["setUuid", "clientUuid"]
	});
	updatev10v11(db, tx);
}

function
updatev10v11(db: IDBDatabase, tx: IDBTransaction)
{
	getAllSetClientsOldRaw(tx).onsuccess = ev => {
		const old = (ev.target as IDBRequest).result
		console.log("old set-clients:", old);

		const req = getCurrentSetRaw(tx);
		req.onsuccess = () => {
			const cs = req.result;
			console.log("db.ts: updatev10v11:", cs);
			if (cs === undefined) { tx.abort(); return; }
			const rs = (ev.target as IDBRequest).result;
			console.log("db.ts:", rs);
			for (const r of rs) {
				const sc = mkSetClientBefore(cs, r);
				console.log("migrating:", sc);
				putSetClientRaw(tx, sc);
			}
			updatev11v12(db, tx);
		};
	}
}

function
updatev11v12(db: IDBDatabase, tx: IDBTransaction)
{
	tx.objectStore(NEW_SET_CLIENTS).createIndex("setUuid", "setUuid");
	updatev12v13(db, tx);
}

function
updatev12v13(db: IDBDatabase, tx: IDBTransaction)
{
	db.deleteObjectStore(TMP_SET_CLIENTS);
	updatev13v14(db, tx);
}

function
updatev13v14(db: IDBDatabase, tx: IDBTransaction)
{
	console.log("updatev13v14");
	const req = tx.objectStore("set-clients").getAll();
	req.onsuccess = () => {
		for (const sc of req.result) {
			const clReq = tx.objectStore("clients").get(sc.clientUuid);
			clReq.onsuccess = () => {
				const cl = clReq.result;
				if (cl === undefined) return;

				console.log(cl, sc.priority);
				cl.priority = sc.priority;
				console.log("db.ts: UPDATE V13 V14:", cl);
				const putReq = tx.objectStore("clients").put(cl);
				putReq.onsuccess = () => {
					console.log("put success", cl.uuid, cl.priority);
				};
				putReq.onerror = () => {
					console.error("put error", putReq.error);
				};
			};
		}
		updatev14v15(db, tx);
	};
}

function
updatev14v15(db: IDBDatabase, tx: IDBTransaction)
{
	const req = tx.objectStore("set-clients").getAll();
	req.onsuccess = () => {
		for (const sc of req.result) {
			delete sc.priority;
			tx.objectStore("set-clients").put(sc); }
		updatev15v16(db, tx);
	};
}

function
updatev15v16(db: IDBDatabase, tx: IDBTransaction)
{
	console.log("db.ts: UPDATE V15 V16");
	db.createObjectStore("permissions", {
		keyPath: ["clientUuid", "publicKey"]
	});

	const clientsReq = tx.objectStore("clients").getAll();
	const accountsReq = tx.objectStore("secret-keys").getAll();
	let clients: ClientSummaryRaw[] = [];
	let accounts: Account[] = [];
	let doneClients = false;
	let doneAccounts = false;

	clientsReq.onsuccess = () => {
		clients = clientsReq.result;
		console.log("clients:", clients);
		doneClients = true;
		checkDone();
	};

	accountsReq.onsuccess = () => {
		accounts = accountsReq.result;
		console.log("accounts:", accounts);
		doneAccounts = true;
		checkDone();
	};

	function
	checkDone()
	{
		if (!doneClients || !doneAccounts) return;

		console.log("db.ts: UPDATE V15 V16: CHECK DONE:", clients, accounts);

		for (const client of clients) {
			for (const account of accounts) {
				const np: Permission =
					noPermission(client.uuid, account.publicKey);
				putPermissionRaw(tx, setPermissionSign(np));
			}
		}
	}
}

export async function
putPermission(p: Permission): Promise<void>
{
	const db = await open();
	const tx = db.transaction("permissions", "readonly");
	putPermissionRaw(tx, p);
}

function
putPermissionRaw(tx: IDBTransaction, p: Permission): Promise<void>
{
	return new Promise((rs, rj) => {
		const store = tx.objectStore("permissions");
		const req = store.put(p);
		req.onsuccess = () => rs();
		req.onerror = () => rj(req.error);
	});
}

export async function
getAllPermissions(): Promise<Permission[]>
{
	const db = await open();
	const tx = db.transaction("permissions", "readonly");
	return getAllPermissionsRaw(tx);
}

function
getAllPermissionsRaw(tx: IDBTransaction): Promise<Permission[]>
{
	return new Promise((rs, rj) => {
		const store = tx.objectStore("permissions");
		const req = store.getAll();
		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});
}

export async function
getPermission(cu: string, pk: Uint8Array): Promise<Permission | undefined>
{
	const db = await open();
	const tx = db.transaction("permissions", "readonly");
	return getPermissionRaw(tx, cu, pk);
}

function
getPermissionRaw(tx: IDBTransaction, cu: string, pk: Uint8Array):
	Promise<Permission | undefined>
{
	return new Promise((rs, rj) => {
		const store = tx.objectStore("permissions");
		const req = store.get([cu, pk] as IDBValidKey);
		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});
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
		const tx = db.transaction(
			[CURRENT_SET, NEW_SET_CLIENTS, NEW_CLIENTS], "readwrite");
		const { name, urlPattern, priority, ...setClient } = client;
		const req = getCurrentSetRaw(tx);
		
		req.onsuccess = () => {
			if (req.result === undefined) { tx.abort(); return; }
			tx.objectStore(NEW_SET_CLIENTS)
				.put(mkSetClient(req.result, setClient));
			tx.objectStore(NEW_CLIENTS).put({
				uuid: client.uuid,
				name: client.name,
				urlPattern: client.urlPattern,
				priority: client.priority }); }

		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error);
	});
}

export async function
putProfile(p: Profile)
{
	const db = await open();
	return new Promise((rs, rj) => {
		const tx = db.transaction(
			[CURRENT_SET, NEW_SET_CLIENTS], "readwrite");
		const req = getCurrentSetRaw(tx);
		req.onsuccess = () => {
			if (req.result === undefined) { tx.abort(); return; }
			tx.objectStore(NEW_SET_CLIENTS)
				.put(mkSetClient(req.result, p)); }
		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error); });
}

export async function
deleteClient(uuid: UUID)
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(
			[CURRENT_SET, NEW_SET_CLIENTS, NEW_CLIENTS], "readwrite" );
		const req = getCurrentSetRaw(tx);

		req.onsuccess = () => {
			const cs = req.result;
			if (cs === undefined) { tx.abort(); return; }
			tx.objectStore(NEW_SET_CLIENTS).delete([cs, fromUuid(uuid)]);
			tx.objectStore(NEW_CLIENTS).delete(fromUuid(uuid));
		}
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
			[CURRENT_SET, NEW_SET_CLIENTS, NEW_CLIENTS], "readonly");
		const req0 = getCurrentSetRaw(tx);
		const rslts: Client[] = [];

		req0.onsuccess = () => {
			const cs = req0.result
			console.log("db.ts: cs:", cs);
			if (cs == undefined) { tx.abort(); return; }
			const req = getAllSetClientsBySetUuidRaw(tx, cs);
			req.onsuccess = () => {
				const scs = req.result;
				console.log("db.ts: scs:", scs);
				for (const sc of scs) {
					const req2 = tx.objectStore(NEW_CLIENTS).get(sc.clientUuid);
					req2.onsuccess = () => {
						const c = req2.result;
						console.log("db.ts: req2.result:", c);
						if (c == undefined) { tx.abort(); return; }
						rslts.push({
							uuid: c.uuid,
							name: c.name,
							urlPattern: c.urlPattern,
							priority: c.priority,
							publicKey: sc.publicKey,
							openSettingsByClick: sc.openSettingsByClick,
							backgroundColor: sc.backgroundColor,
							backgroundOpacity: sc.backgroundOpacity,
							positionX: sc.positionX,
							positionY: sc.positionY,
							displayAccount: sc.displayAccount
						});
					};
					req2.onerror = () => rj(req2.error);
				}
			};
			req.onerror = () => rj(req.error);
		};
		tx.oncomplete = () => {
			console.log("db.ts: getClients:", rslts);
			rs(rslts); }
		tx.onabort = () => rj(
			tx.error ?? new Error("Transaction aborted"));
		tx.onerror = () => rj(
			tx.error ?? new Error("Transaction failed"));
		req0.onerror = () => rj(req0.error);
	});
}

export async function
getAllClients(): Promise<ClientSummaryRaw[]>
{
	const db = await open();
	return new Promise((rs, rj) => {
		const tx = db.transaction([NEW_CLIENTS], "readonly");
		const req = tx.objectStore(NEW_CLIENTS).getAll();
		tx.oncomplete = () => {
			console.log("db.ts: getAllClients:", req.result);
			rs(req.result); }
		tx.onerror = () => rj(tx.error); });
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
getClientSummary(uuid: string): Promise<ClientSummary>
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction(NEW_CLIENTS, "readonly");
		const req = tx.objectStore(NEW_CLIENTS).get(uuid);
		req.onsuccess = () => { rs(req.result); };
		req.onerror = () => rj(req.error);
	});
}

export async function
putClientNoProfile(clnt: ClientSummaryRaw)
{
	const db = await open();
	return new Promise((rs, rj) => {
		const tx = db.transaction([NEW_CLIENTS], "readwrite");
		tx.objectStore(NEW_CLIENTS).put(clnt);
		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error); });
}

export async function
getClient(uuid: string): Promise<EditingClient>
{
	const db = await open()

	return new Promise((rs, rj) => {
		const tx = db.transaction(
			[CURRENT_SET, NEW_SET_CLIENTS, NEW_CLIENTS], "readonly");
		const req0 = getCurrentSetRaw(tx);
		const req2 = tx.objectStore(NEW_CLIENTS).get(uuid);
		let r: Client | undefined;
		let r2: ClientSummaryRaw | undefined;
		let done1 = false;
		let done2 = false;

		req0.onsuccess = () => {
			const cs = req0.result;
			if (cs === undefined) { tx.abort(); return; }
			const req = tx.objectStore(NEW_SET_CLIENTS).get([cs, uuid]);
			req.onsuccess = () => {
				r = req.result;
				done1 = true;
				finish();
			}
		}

		req2.onsuccess = () => {
			r2 = req2.result;
			console.log("db.ts: REQ 2: RESULT:", r2);
			done2 = true;
			finish();
		}

		function finish() {
			if (!done1 || !done2) return;
			if (r === undefined || r2 === undefined) {
				rj(new Error(`no such client: ${uuid}`));
				return;
			}

				rs({
					uuid: r2.uuid,
					name: r2.name,
					urlPattern: r2.urlPattern,
					priority: r2.priority,
					publicKey: r.publicKey,
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
		req0.onerror = () => rj(req0.error);
		req2.onerror = () => rj(req2.error);
		tx.onabort = () => rj(
			tx.error ?? new Error("Transaction aborted"));
		tx.onerror = () => rj(
			tx.error ?? new Error("Transaction failed"));
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

export async function
getSet(uuid: string)
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction("sets", "readonly");
		const req = tx.objectStore("sets").get(uuid);
		tx.oncomplete = () => rs(req.result);
		tx.onerror = () => rj(tx.error); });
}

export async function
getSetList(): Promise<ClientSet[]>
{
	const db = await open();
	return new Promise((rs, rj) => {
		const tx = db.transaction("sets", "readonly");
		const req = tx.objectStore("sets").getAll();
		tx.oncomplete = () => rs(req.result);
		tx.onerror = () => rj(tx.error); });
}

export async function
putSet(set: ClientSet)
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction("sets", "readwrite");
		putSetRaw(tx, set);
		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error); });
}

export async function
getCurrentSet(): Promise<string | undefined>
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction("current-set", "readonly");
		const req = tx.objectStore("current-set").get(unit);
		tx.oncomplete = () => rs(req.result);
		tx.onerror = () => rj(tx.error); });
}

function
getCurrentSetRaw(tx: IDBTransaction): IDBRequest<string | undefined>
{
	return tx.objectStore("current-set").get(unit);
}

export async function
putCurrentSet(uuid: string)
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction("current-set", "readwrite");
		putCurrentSetRaw(tx, uuid);
		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error); });
}

function
initializeSet(tx: IDBTransaction)
{
	const uuid = crypto.randomUUID();
	putSetRaw(tx, { uuid: uuid, name: "default" });
	putCurrentSetRaw(tx, uuid);
}

function
putSetRaw(tx: IDBTransaction, set: ClientSet)
{
	tx.objectStore("sets").put(set);
}

function
putCurrentSetRaw(tx: IDBTransaction, uuid: string)
{
	tx.objectStore("current-set").put(uuid, unit);
}

type SetClientBefore = {
	setUuid: string,
	clientUuid: string,
	publicKey: Uint8Array,
	priority: number,
	openSettingsByClick: boolean,
	backgroundColor: string, backgroundOpacity: number,
	positionX: number, positionY: number,
	displayAccount: boolean }

type SetClient = {
	setUuid: string,
	clientUuid: string,
	publicKey: Uint8Array,
	openSettingsByClick: boolean,
	backgroundColor: string, backgroundOpacity: number,
	positionX: number, positionY: number,
	displayAccount: boolean }

type SetClientOldBefore = {
	uuid: string,
	publicKey: Uint8Array,
	priority: number,
	openSettingsByClick: boolean,
	backgroundColor: string, backgroundOpacity: number,
	positionX: number, positionY: number,
	displayAccount: boolean }

type SetClientOld = {
	uuid: string,
	publicKey: Uint8Array,
	openSettingsByClick: boolean,
	backgroundColor: string, backgroundOpacity: number,
	positionX: number, positionY: number,
	displayAccount: boolean }

function
mkSetClientBefore(setUuid: string, old: SetClientOldBefore): SetClientBefore
{
	return {
		setUuid: setUuid,
		clientUuid: old.uuid,
		publicKey: old.publicKey,
		priority: old.priority,
		openSettingsByClick: old.openSettingsByClick,
		backgroundColor: old.backgroundColor,
		backgroundOpacity: old.backgroundOpacity,
		positionX: old.positionX, positionY: old.positionY,
		displayAccount: old.displayAccount };
}

function
mkSetClient(setUuid: string, old: SetClientOld): SetClient
{
	return {
		setUuid: setUuid,
		clientUuid: old.uuid,
		publicKey: old.publicKey,
		openSettingsByClick: old.openSettingsByClick,
		backgroundColor: old.backgroundColor,
		backgroundOpacity: old.backgroundOpacity,
		positionX: old.positionX, positionY: old.positionY,
		displayAccount: old.displayAccount };
}

export async function
putSetClient(setClient: SetClient)
{
	const db = await open();
	return new Promise((rs, rj) => {
		const tx = db.transaction(NEW_SET_CLIENTS, "readwrite");
		putSetClientRaw(tx, setClient);
		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error); });
}

function
putSetClientRaw(tx: IDBTransaction, setClient: SetClient)
{
	tx.objectStore(NEW_SET_CLIENTS).put(setClient);
}

function
getAllSetClientsOldRaw(tx: IDBTransaction): IDBRequest<SetClientOld[]>
{
	return tx.objectStore(TMP_SET_CLIENTS).getAll();
}

function
getAllSetClientsBySetUuidRaw(
	tx: IDBTransaction,
	setUuid: string ): IDBRequest<SetClient[]>
{
	return tx.objectStore(NEW_SET_CLIENTS)
		.index("setUuid").getAll(IDBKeyRange.only(setUuid));
}

import { scrypt } from "@noble/hashes/scrypt.js"

export async function
addUser(idb, uid, pswd)
{
	const slt = crypto.getRandomValues(new Uint8Array(16));
	const hsh = await scrypt(
		new TextEncoder().encode(pswd), slt,
		{ N: 2 ** 16, r: 8, p: 1, dkLen: 32 } );
	const db = await openDB(idb);
	try {	const tx = db.transaction("users", "readwrite");
		tx.objectStore("users")
			.add({ userId: uid, salt: slt, hash: hsh });
		await txDone(tx); }
	finally { db.close(); }
}

export async function
login(idb, uid, pswd)
{
	let user;
	const db = await openDB(idb);
	try {	const tx = db.transaction("users", "readonly");
		const req = tx.objectStore("users").get(uid);
		user = await new Promise((rslv, rjct) => {
			req.onsuccess = () => rslv(req.result);
			req.onerror = () => rjct(req.error); });
		await txDone(tx); }
	finally { db.close(); }
	if (!user) return false;
	const hash = await scrypt(
		new TextEncoder().encode(pswd), user.salt,
		{ N: 2 ** 16, r: 8, p: 1, dkLen: 32 });
	return equal(hash, user.hash);

	function
	equal(a, b)
	{
		if (a.length !== b.length) return false;
		let dff = 0;
		for (let i = 0; i < a.length; i++) dff |= a[i] ^ b[i];
		return dff === 0;
	}
}

function
openDB(idb)
{
	return new Promise((rslv, rjct) => {
		const req = idb.open("login", 1);
		req.onupgradeneeded = () => {
			req.result.createObjectStore(
				"users", { keyPath: "userId" } ); };
		req.onsuccess = () => rslv(req.result);
		req.onerror = () => rjct(req.error); });
}

function
txDone(tx)
{
	return new Promise((rslv, rjct) => {
		tx.oncomplete = rslv;
		tx.onerror = () => rjct(tx.error);
		tx.onabort = () => rjct(tx.error); });
}

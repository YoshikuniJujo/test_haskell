import "fake-indexeddb/auto";
import { scrypt } from "@noble/hashes/scrypt.js"

console.log("FAKE FAKE FAKE");

export async function
addUser(idb, userId, password)
{
	const salt = crypto.getRandomValues(new Uint8Array(16));

	const db = await openDB(idb);
	const hash = await scrypt(
		new TextEncoder().encode(password),
		salt,
		{ N: 2 ** 16, r: 8, p: 1, dkLen: 32 }
	);

	const tx = db.transaction("users", "readwrite");
	tx.objectStore("users").add({ userId, salt, hash });
	await txDone(tx);
	db.close()
}

function
openDB(idb)
{
	return new Promise((rslv, rjct) => {
		const req = idb.open("login", 1);

		req.onupgradeneeded = () => {
			req.result.createObjectStore("users", {
				keyPath: "userId"
			});
		};

		req.onsuccess = () => rslv(req.result);
		req.onerror = () => rjct(req.error);
	});
}

function
txDone(tx)
{
	return new Promise((rslv, rjct) => {
		tx.oncomplete = rslv;
		tx.onerror = () => rjct(tx.error);
		tx.onabort = () => rjct(tx.error);
	});
}

export async function
login(idb, userId, password)
{
	const db = await openDB(idb);

	try {
		const tx = db.transaction("users", "readonly");
		const req = tx.objectStore("users").get(userId);
		const user = await new Promise((rslv, rjct) => {
			req.onsuccess = () => rslv(req.result);
			req.onerror = () => rjct(req.error);
		});
		await txDone(tx);

		if (!user) return false;

		const hash = await scrypt(
			new TextEncoder().encode(password),
			user.salt,
			{ N: 2 ** 16, r: 8, p: 1, dkLen: 32 }
		);

		return equal(hash, user.hash);
	}
	finally {
		db.close();
	}
}

function
equal(a, b)
{
	if (a.length !== b.length) return false;
	let diff = 0;
	for (let i = 0; i < a.length; i++) diff |= a[i] ^ b[i];
	return diff === 0;
}

await addUser(indexedDB, "tarou", "foobar");

const db = await openDB(indexedDB);
const tx = db.transaction("users", "readonly");
const req = tx.objectStore("users").get("tarou");
req.onsuccess = () => console.log(req.result);
await txDone(tx);
db.close()

console.log(await login(indexedDB, "tarou", "foobar"));
console.log(await login(indexedDB, "tarou", "foobarbaz"));

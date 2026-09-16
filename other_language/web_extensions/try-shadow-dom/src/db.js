let dbPromise;

function
open()
{
	console.log("open: ", dbPromise);
	if (dbPromise) return dbPromise;

	console.log("open: before Promise");
	dbPromise = new Promise((rs, rj) => {
		const req = indexedDB.open("try-shadow-dom", 1);
		req.onupgradeneeded = () => {
			const db = req.result;
			if (!db.objectStoreNames.contains("AccountDisplayInfo"))
				db.createObjectStore(
					"AccountDisplayInfo", { keyPath: "uuid" } );
		};

		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);

	});
	console.log("open: after Promise");

	return dbPromise;
}

export async function
putAccountDisplayInfo(adi)
{
	console.log("before open");
	const db = await open();
	console.log("after open: ", db);

	console.log("before put");
	return new Promise((rs, rj) => {
		const tx = db.transaction("AccountDisplayInfo", "readwrite");
		tx.objectStore("AccountDisplayInfo").put(adi);
		tx.oncomplete = rs;
		tx.onerror = () => rj(tx.error);
	});
}

export async function
getAccountDisplayInfo()
{
	const db = await open();

	return new Promise((rs, rj) => {
		const tx = db.transaction("AccountDisplayInfo", "readonly");
		const req = tx.objectStore("AccountDisplayInfo").getAll();

		req.onsuccess = () => rs(req.result);
		req.onerror = () => rj(req.error);
	});
}

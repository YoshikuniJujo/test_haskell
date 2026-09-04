import { addUser, login } from "./try-indexeddb/login.js"

console.log("background.js");

browser.runtime.onMessage.addListener( async m => {
	console.log("message received", m);
	switch (m.type) {
		case "addUser":
			await addUser(indexedDB, m.userId, m.password);
			break;
		case "login":
			const rslt = await login(indexedDB, m.userId, m.password);
			console.log(rslt);
			break;
	}
});

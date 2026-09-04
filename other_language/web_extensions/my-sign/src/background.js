import { addUser, login } from "./try-indexeddb/login.js"

console.log("background.js");

browser.runtime.onMessage.addListener( async m => {
	console.log("message received", m);
	switch (m.type) {
		case "addUser":
			return addUser(indexedDB, m.userId, m.password);
		case "login":
			return login(indexedDB, m.userId, m.password);
	}
});

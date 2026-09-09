import { addUser, login } from "./try-indexeddb/login.js"

console.log("background.js");

browser.runtime.onMessage.addListener( async (m, s) => {
	console.log("message received", m);
	switch (m.type) {
		case "addUser":
			return addUser(indexedDB, m.userId, m.password);
		case "login":
			return login(indexedDB, m.userId, m.password);
		case "get-account":
			console.log(s.url);
			return "dummy account";
		case "get-public-key":
			console.log("background: get-public-key");
			return "background: " + s.url;
	}
});

import { addUser, login } from "./try-indexeddb/login.js"
import * as DB from "./db.js"

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

			console.log(typeof s.url, s.url);
			const url = String(s.url);
			const clients = await DB.getClients();
			for (const client of clients) {
				const pattern = new URLPattern(client.urlPattern);
				if (pattern.test(url)) return hex(client.publicKey);
			}

			return "background: " + s.url;
	}
});

function
hex(bs)
{
	return [...bs]
		.map(b => b.toString(16).padStart(2, "0")).join("");
}

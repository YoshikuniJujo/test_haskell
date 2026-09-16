import { getAccountDisplayInfo, listAccountDisplayInfos } from "./db.js"

console.log("background");

browser.runtime.onMessage.addListener( async (m, s) => {
	console.log("message received", m);
	switch (m.method) {
		case "accountDisplayInfo":
			const foo = await listAccountDisplayInfos();
			console.log(foo);
			const bar = await getAccountDisplayInfo("0123456789");
			console.log(bar);
			return bar ?? { positionX: 50, positionY: 50 };
	}
});

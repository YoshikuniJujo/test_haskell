console.log("BACKGROUND");

type FooBarMessage = {
	method: "foobar";
};

browser.runtime.onMessage.addListener((m: FooBarMessage, s: browser.runtime.MessageSender) => {
	console.log("background.js:", m, s);
	switch (m.method) {
		case "foobar": return (async () => {
			console.log("background:", m, s);
			return 123; })();
	}
});

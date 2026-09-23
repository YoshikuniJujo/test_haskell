console.log("OPTIONS BEGIN");

browser.runtime.sendMessage( { method: "hello" } );

browser.runtime.onMessage.addListener((m, s) => {
	console.log("options.js: recieve message");
	console.log(m);
	console.log(s);
});

const openInTab = document.querySelector("#open-in-tab");

openInTab.addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "openOptionsInTab" });
});

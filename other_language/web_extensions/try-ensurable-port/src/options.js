console.log("OPTIONS BEGIN");

console.log(location.search);

let openType;
let pageId;

if (location.search === "") {
	openType = "browser";
	pageId = "browser"; }
else {
	const params = new URLSearchParams(location.search);
	openType = params.get("openType");
	pageId = params.get("pageId");
}

browser.runtime.sendMessage( { method: "hello" } );

browser.runtime.onMessage.addListener((m, s) => {
	console.log("options.js: recieve message");
	console.log(m);
	console.log(s);
});

const openInTab = document.querySelector("#open-in-tab");

if (openType === "browser") openInTab.hidden = false;

openInTab.addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "openOptionsInTab" });
});

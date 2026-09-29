console.log("FOOBARBAZ");

let openType = ""; let pageId = "";

if (location.search === "") { openType = "browser"; pageId = "browser"; }
else {
	const param = new URLSearchParams(location.search);
	openType = param.get("openType");
	pageId = param.get("pageId");
}

const timeoutTest = document.querySelector("#timeout-test");
timeoutTest.addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "timeoutTest" });
	const listener = m => {
		console.log(m);
		const p = browser.runtime.connect({ name: "foobarbaz" });
		console.log(p);
		setTimeout(() => {
			p.onMessage.addListener(m => {
				console.log(m);
			});
			p.disconnect();
			browser.runtime.onMessage.removeListener(listener);
		}, 0);
	};
	browser.runtime.onMessage.addListener(listener);
});

const postBeforeAddListener = document.querySelector("#post-before-add-listener");
postBeforeAddListener.addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "postBeforeAddListener" });
	const listener = m => {
		console.log("postBeforeAddListener:", m);
		const p = browser.runtime.connect({ name: "postBeforeAddListener" });
		console.log("OPTIONS:", p);
		p.postMessage("HOGEPIYO1 from OPTIONS");
		p.postMessage("HOGEPIYO2 from OPTIONS");
		p.postMessage("HOGEPIYO3 from OPTIONS");
		p.postMessage("HOGEPIYO4 from OPTIONS");
		p.postMessage("HOGEPIYO5 from OPTIONS");
		p.onMessage.addListener(m => {
			console.log("OPTIONS:", m);
		});
	};
	browser.runtime.onMessage.addListener(listener);
});

const openInTab = document.querySelector("#open-in-tab");
if (openType == "browser") openInTab.hidden = false;
openInTab.addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "openOptionsInTab" }); });

console.log("OPTIONS BEGIN");

document.querySelector("#open-tab").addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "openTab" });
});

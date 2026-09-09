document.documentElement.style.border = "5px solid green";

const div = document.createElement("div");
Object.assign(div.style, {
	position: "fixed",
	top: "10px",
	right: "10px",
	padding: "8px 12px",
	background: "rgba(0, 128, 0, 0.5)",
	color: "white",
	zIndex: "2147483647"
});

(async () => {
	const client = await browser.runtime.sendMessage({ method: "get-account" });
	div.textContent = client;
})();

document.body.append(div);

const nostr = {

	getPublicKey()
	{
		return new window.Promise(async (rs, rj) => {
			try {
				const v = await browser.runtime.sendMessage({
					method: "get-public-key"
				});
				rs(v);
			}
			catch (e) { rj(cloneInto(e, window)); }
		});
	},

	signEvent(ev)
	{
		return new window.Promise(rs =>
			rs(ev));
	}
}

window.wrappedJSObject.nostr =
	cloneInto(nostr, window, { cloneFunctions: true });

document.documentElement.style.border = "5px solid green";

const div = document.createElement("div");
div.textContent = "my-sign";
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
	const client = await browser.runtime.sendMessage({ type: "get-account" });
	div.textContent = client;
})();

document.body.append(div);

mySign = {

	addUser(uid, pswd)
	{
		console.log("mySign.addUser", uid);
		return new window.Promise((rslv, rjct) => {
			browser.runtime.sendMessage({
				type: "addUser", userId: uid, password: pswd }).then(rslv, rjct);
		});
	},

	login(uid, pswd)
	{
		console.log("mySign.login", uid);
		return new window.Promise(async (rslv, rjct) => {
			try {
				const v = await browser.runtime.sendMessage({
					type: "login", userId: uid, password: pswd });
				rslv(v);
			}
			catch (err) { rjct(err); }
		});
	}

};

window.wrappedJSObject.mySign =
	cloneInto(mySign, window, { cloneFunctions: true });

const nostr = {

	getPublicKey()
	{
		return new window.Promise(async (rs, rj) => {
			try {
				const v = await browser.runtime.sendMessage({
					type: "get-public-key"
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

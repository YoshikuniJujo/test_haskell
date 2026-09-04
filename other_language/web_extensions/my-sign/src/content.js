document.documentElement.style.border = "5px solid green";

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

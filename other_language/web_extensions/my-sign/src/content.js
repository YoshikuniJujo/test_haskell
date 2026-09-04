document.documentElement.style.border = "5px solid green";

window.mySign = {

	addUser(uid, pswd)
	{
		console.log("mySign.addUser", uid);
	},

	login(uid, pswd)
	{
		console.log("mySign.login", uid);
	}

};

window.wrappedJSObject.mySign =
	cloneInto(mySign, window, { cloneFunctions: true });

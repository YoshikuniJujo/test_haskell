document.body.style.border = "3px solid green";

const test = {

	async openInputTab() {

		const r = await browser.runtime.sendMessage({
			method: "openInputTab"
		});

		console.log(r);

	}

};

window.wrappedJSObject.test =
	cloneInto(test, window, {
		cloneFunctions: true
	});
